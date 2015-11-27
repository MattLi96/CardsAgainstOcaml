open Async.Std
open Cohttp
open Cohttp_async
open State
open Model
open Timer
open Heartbeat

(* compile with: $ corebuild receive_post.native -pkg cohttp.async *)

(*Additional state for helping run the server*)
type a_state = {
  (*Fill when phase is over, either due to time or everyone played*)
  mutable phase_over: unit Ivar.t;

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
    (*TODO: make sure this is legit, could be issue involving phase_over*)
    a_state.timer <- create_timer 40; (*TODO: change 40 later*)
    bind_timer a_state.timer (Ivar.fill_if_empty a_state.phase_over);
    start_timer a_state.timer;

    let _ = (Ivar.read a_state.phase_over) >>= (fun _ ->
        a_state.phase_over <- Ivar.create ();
        s_state := game_next_phase (!s_state);
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
    Log.Global.info "POST Body: %s" body;
    Log.Global.info "uID found is %i" uID;
    Log.Global.info "type found is %s" typ;
    if (typ = "play") then
      let new_state = user_play_white (!s_state) uID body in
      s_state := new_state;
      Server.respond `OK
    else
    if (typ = "judge") then
      let new_state = user_judge (!s_state) uID body in
      s_state := new_state;
      Server.respond `OK
    else failwith "error"

(*TODO pass in other parameters in the header of the response*)
let respond_get f_state body req =
  match f_state with
  | (a_state, s_state) ->
    let l_headers = (Cohttp.Request.headers req) in
    let uID = get_UID (Header.to_list (l_headers)) in
    Log.Global.info "uID found is %i" (uID);
    let ans = get_univ_c (State.get_user_state (!s_state) uID) in

    let s_played = (State.string_played (ans.played)) in
    let s_black = (ans.b_card) in
    let s_scores = (State.string_scores (ans.scores)) in
    let s_winners = (State.string_winners (ans.winners)) in
    let s_hand = (State.string_hand (ans.hand)) in

    Log.Global.info "played: %s" (State.string_played (ans.played));
    Log.Global.info "black: %s" (ans.b_card);
    Log.Global.info "scores: %s" (State.string_scores (ans.scores));
    Log.Global.info "winners: %s" (State.string_winners (ans.winners));
    Log.Global.info "hand: %s" (State.string_hand (ans.hand));

    let temp_header = Header.add (Header.init()) "b_card"  (s_black) in
    let h2 = Header.add (temp_header) "played" s_played in
    let h3 = Header.add (h2) "scores" s_scores in
    let h4 = Header.add (h3) "winners" s_winners in
    let h5 = Header.add (h4) "hand" s_hand in
   (*    type univ_c_state = {
    played  : (uID * white_card) list;
    b_card  : black_card;
    scores  : scores;
    winners : (black_card * white_card * uID) option;
    hand    : white_card list;
  } *)
    Server.respond `OK ~headers: h5

let respond_put (f_state:full_state) body req =
  match f_state with
  | (a_state, s_state) ->
    if (body = "start") then (
      s_state := game_start !s_state;
      let _ = gameloop f_state in
      (Log.Global.info "Start %s" "triggered";
       Server.respond `OK)
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
                hb = create_heartbeat 5; (*Heartbeat cycle set for 5 seconds*)
                timer = create_timer 0}
              , ref (init_s_state ())) in
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Try 'curl -X POST -d 'foo bar' http://localhost:%d\n" port;
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
