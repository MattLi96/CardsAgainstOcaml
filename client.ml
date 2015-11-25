(*Client*)
open Async.Std
open Lwt
open Cohttp
open Cohttp_lwt_unix
open State

type state = c_state

(*For sending a test request: obtained from the cohttp github*)
let test =
  Client.post (Uri.of_string "http://localhost:8000/") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

(*play_white allows a user to play a card*)
(* val play_white: uID -> white_card -> unit *)
let play_white (uID:uID) (white:white_card) =
  let new_header = Header.init() in
  let new_header2 = Header.add new_header "uID" (string_of_int uID) in
  let new_header3 = Header.add new_header2 "white" white in
  Client.post (Uri.of_string "http://localhost:8000/") ~headers: new_header3 >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  ignore(body); ()

(*judge allows a user to select the winner of a round if he is the judge*)
(* val judge: uID -> white_card -> unit *)
let judge uID white =
  let new_header = Header.init() in
  let new_header2 = Header.add new_header "uID" (string_of_int uID) in
  let new_header3 = Header.add new_header2 "white" white in
  Client.post (Uri.of_string "http://localhost:8000/") ~headers: new_header3 >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  ignore(body); ()

(*get_user_state returns the state of the user*)
(* val get_user_state: uID -> state *)
let get_user_state uID = failwith "unimplemented"

(*start_heatbeat creates an ivar that represents the heartbeat*)
(* val start_heartbeat: uID -> 'a Async.Std.Ivar.t *)

let start_heartbeat uID =
  let ivar = Ivar.create () in
  ivar

(*check_heartbeat returns false if the heartbeat ever fails*)
(* val check_heartbeat: uID -> boolean *)
let check_heartbeat uID = failwith "unimplemented"
