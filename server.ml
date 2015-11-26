open Async.Std
open Cohttp
open Cohttp_async
open State
open Model

(* compile with: $ corebuild receive_post.native -pkg cohttp.async *)

(*boolean is true when round should be over*)
type game_state = bool ref * State.s_state ref

(*the state is a game_state*)
let gameloop state = failwith "TODO"

let rec get_UID l =
  (match l with
  | [] -> failwith "no uID"
  | h::t -> if (fst h = "uid") then int_of_string (snd h) else get_UID t)

let rec get_type l =
  (match l with
  | [] -> failwith "no type"
  | h::t -> if (fst h = "type") then snd h else get_type t)

let respond_post state body req =
  let l_headers = (Cohttp.Request.headers req) in
  let uID = get_UID (Header.to_list (l_headers)) in
  let typ = get_type (Header.to_list l_headers) in
  Log.Global.info "POST Body: %s" body;
  Log.Global.info "uID found is %i" uID;
  Log.Global.info "type found is %s" typ;
  if (typ = "play") then
    let new_state = user_play_white (!state) uID body in
    state := new_state;
    Server.respond `OK
  else
  if (typ = "judge") then
    let new_state = user_judge (!state) uID body in
    state := new_state;
    Server.respond `OK
  else failwith "error"

let respond_get state body req =
  let l_headers = (Cohttp.Request.headers req) in
  Log.Global.info "GET Body: %s" body;
  Log.Global.info "uID found is %i" (get_UID (Header.to_list (l_headers)));
  Server.respond `OK

let start_server port () =
  let state = ref (init_s_state ()) in 
  (*let gs = (ref false, state) in*) (*add in later, start a game loop thread*)
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Try 'curl -X POST -d 'foo bar' http://localhost:%d\n" port;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) (fun ~body _ req ->
      match req |> Cohttp.Request.meth with
      | `POST -> (Body.to_string body) >>= (fun body -> respond_post state body req)
      | `GET -> (Body.to_string body) >>= (fun body -> respond_get state body req)
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
