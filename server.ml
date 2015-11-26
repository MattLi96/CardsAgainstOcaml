open Async.Std
open Cohttp
open Cohttp_async
open State

(* compile with: $ corebuild receive_post.native -pkg cohttp.async *)

(* let game_state = ref (init_s_state ()) *)

let rec get_UID l =
  (match l with
  | [] -> failwith "no uID"
  | h::t -> if (fst h = "uid") then int_of_string (snd h) else get_UID t)

let rec get_type l =
  (match l with
  | [] -> failwith "no type"
  | h::t -> if (fst h = "type") then snd h else get_type t)

let respond_post body req =
  let l_headers = (Cohttp.Request.headers req) in
  Log.Global.info "POST Body: %s" body;
  Log.Global.info "uID found is %i" (get_UID (Header.to_list (l_headers)));
  Log.Global.info "type found is %s" (get_type (Header.to_list l_headers));
  Server.respond `OK

let respond_get body req =
  let l_headers = (Cohttp.Request.headers req) in
  Log.Global.info "GET Body: %s" body;
  Log.Global.info "uID found is %i" (get_UID (Header.to_list (l_headers)));
  Server.respond `OK
let start_server port () =
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Try 'curl -X POST -d 'foo bar' http://localhost:%d\n" port;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) (fun ~body _ req ->
      match req |> Cohttp.Request.meth with
      | `POST -> (Body.to_string body) >>= (fun body -> respond_post body req)
      | `GET -> (Body.to_string body) >>= (fun body -> respond_get body req)
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