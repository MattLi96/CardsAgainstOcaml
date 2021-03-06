open Async.Std
open Cohttp
open Cohttp_async
open State
open Model
open Timer
open Heartbeat

(*Additional state for helping run the server*)
type a_state = {
  (*Fill when phase is over, either due to time or everyone played*)
  mutable phase_over: unit Ivar.t;
  mutable started: bool;
  (*Heartbeat abbreviated for simplicity
    Heartbeat should not need to be reinitialized ever.
    A timer is only for the current phase. A new one is created for each loop.
  *)
  hb : heartbeat;
  mutable timer : timer;
}

(*All the state that should be required to run a server. Abbreviated as f_state
  in most functions
*)
type full_state = a_state * State.s_state ref

(*The main loop for the game. One loop is one turn*)
let rec gameloop f_state =
  match f_state with
  | (a_state, s_state) ->
    a_state.timer <- create_timer 40;
    bind_timer a_state.timer (Ivar.fill_if_empty a_state.phase_over);
    start_timer a_state.timer;

    let _ = (Ivar.read a_state.phase_over) >>= (fun _ ->
        a_state.phase_over <- Ivar.create ();
        s_state := game_next_phase (!s_state) (get_active_users a_state.hb);
        gameloop f_state) in
    return ()


(*Server functions*)
let rec get_UID l =
  (match l with
   | [] -> failwith "no uID"
   | h::t -> if (fst h = "uid") then int_of_string (snd h) else get_UID t)

let rec get_type l =
  (match l with
   | [] -> failwith "no type"
   | h::t -> if (fst h = "type") then snd h else get_type t)

let rec get_param l str =
  (match l with
   | [] -> failwith ("no " ^ str)
   | h::t -> if (fst h = str) then snd h else get_param t str)

(*TODO: alter post to fill the a_state.phase_over when turn is done*)
let respond_post f_state body req =
  match f_state with
  | (a_state, s_state) -> (*note ivar and state are references*)
    let l_headers = (Cohttp.Request.headers req) in
    let uID = get_UID (Header.to_list (l_headers)) in
    let typ = get_type (Header.to_list l_headers) in
    (*     Uncomment below for log statements
           Log.Global.info "POST Body: %s" body;
           Log.Global.info "uID found is %i" uID;
           Log.Global.info "type found is %s" typ; *)
    match typ with
    | "play" ->
      let new_state = user_play_white (!s_state) uID body in
      s_state := new_state;
      (if play_state_finished !s_state then
         Ivar.fill_if_empty a_state.phase_over ()
       else ()
      );
      Server.respond `OK
    | "judge" ->
      (Log.Global.info "JUDGE: %i" (get_univ_s (!s_state)).judge);
      if (uID = (get_univ_s (!s_state)).judge) then
        let new_state = user_judge (!s_state) uID body in
        s_state := new_state;
        (if judge_state_finished !s_state then
           Ivar.fill_if_empty a_state.phase_over ()
         else ()
        );
        Server.respond `OK
      else
        Server.respond `Found
    | "pause" -> pause_timer a_state.timer; Server.respond `OK
    | "resume" -> start_timer a_state.timer; Server.respond `OK
    | "beat" -> let time = get_time a_state.timer in
      let h = Header.add (Header.init()) "time"  (string_of_int time) in
      Heartbeat.beat a_state.hb uID;
      (* (Log.Global.info "CLIENT HEARTBEAT: %i" uID); *)
      Server.respond `OK ~headers: h
    | _ -> failwith "wierd type of post"

(*Other parameters in the header of the response*)
let respond_get f_state body req =
  match f_state with
  | (a_state, s_state) ->
    let l_headers = (Cohttp.Request.headers req) in
    let uID = get_UID (Header.to_list (l_headers)) in
    Log.Global.info "uID found is %i" (uID);
    let cstate = State.get_user_state (!s_state) uID in
    let ans = get_univ_c cstate in

    let s_played = (State.string_played (ans.played)) in
    let s_black = (ans.b_card) in
    let s_scores = (State.string_scores (ans.scores)) in
    let s_winners = (State.string_winners (ans.winners)) in
    let s_hand = (State.string_hand (ans.hand)) in
    let s_gstate = State.string_state cstate in

    Log.Global.info "played: %s" (State.string_played (ans.played));
    Log.Global.info "black: %s" (ans.b_card);
    Log.Global.info "scores: %s" (State.string_scores (ans.scores));
    Log.Global.info "winners: %s" (State.string_winners (ans.winners));
    Log.Global.info "hand: %s" (State.string_hand (ans.hand));
    (*this has the new line, should be the last info printed*)
    Log.Global.info "judge: %i\n" ((get_univ_s !s_state).judge);

    let temp_header = Header.add (Header.init()) "b_card"  (s_black) in
    let h2 = Header.add (temp_header) "played" s_played in
    let h3 = Header.add (h2) "scores" s_scores in
    let h4 = Header.add (h3) "winners" s_winners in
    let h5 = Header.add (h4) "hand" s_hand in
    let h6 = Header.add (h5) "state" s_gstate in
    Server.respond `OK ~headers: h6

let respond_put (f_state:full_state) body req =
  match f_state with
  | (a_state, s_state) ->
    if (body = "start") then (
      if (not a_state.started) then
        (a_state.started <- true;
         s_state := game_start !s_state (get_active_users a_state.hb);
         let _ = gameloop f_state in
         (Log.Global.info "Start %s" "triggered";
          Server.respond `OK))
      else Server.respond `Found
    )else
      (let l_headers = (Cohttp.Request.headers req) in
       let name = get_param (Header.to_list (l_headers)) "name" in
       Log.Global.info "name is %s" (name);
       let logic = user_add !s_state name in
       s_state := (snd logic);
       let temp_header = Header.add (Header.init()) "uID"
           (string_of_int (fst logic)) in
       (Server.respond `OK ~headers: temp_header))

let start_server port () =
  let state = ({phase_over = Ivar.create ();
                started = false;
                hb = create_heartbeat 5; (*Heartbeat cycle set for 5 seconds*)
                timer = create_timer 0}
              , ref (init_s_state ())) in
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Try (this will crash the server):
           curl -X POST -d 'foo bar' http://localhost:%d\n" port;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) (fun ~body _ req ->
        match req |> Cohttp.Request.meth with
        | `POST -> (Body.to_string body) >>= (fun body ->
            respond_post state body req)
        | `GET -> (Body.to_string body) >>= (fun body ->
            respond_get state body req)
        | `PUT -> (Body.to_string body) >>= (fun body ->
            respond_put state body req)
        | _ -> Server.respond `Method_not_allowed
      )
  >>= fun _ -> Deferred.never ()

let () =
  Command.async_basic
    ~summary:"Simple http server"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                 ) start_server
  |> Command.run
