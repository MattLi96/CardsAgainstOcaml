open Async.Std
open Cohttp_async
open State

(* compile with: $ corebuild receive_post.native -pkg cohttp.async *)

let game_state = ref (init_s_state ())

let respond_post body req =
  failwith "unimplemented"

let respond_get body req =
  failwith "unimplemented"

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
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                 ) start_server
  |> Command.run