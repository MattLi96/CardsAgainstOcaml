(*Client*)
open Async.Std
open Cohttp
open Cohttp_async
open State

type state = c_state

(*play_white allows a user to play a card*)
(* val play_white: uID -> white_card -> unit *)
let play_white (uID:uID) (white:white_card) =
  let temp_header = Header.add (Header.init()) "uID" (string_of_int uID) in
  let temp_header_with_type = Header.add temp_header "type" "play" in
  let temp_body = Body.of_string white in
  let post_req = (Client.post (Uri.of_string "http://localhost:8080/") ~headers:temp_header_with_type
    ~body:temp_body) in
  post_req >>= (fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  return ())

(*judge allows a user to select the winner of a round if he is the judge*)
(* val judge: uID -> white_card -> unit *)
let judge uID white =
  let temp_header = Header.add (Header.init()) "uID" (string_of_int uID) in
  let temp_header_with_type = Header.add temp_header "type" "judge" in
  let temp_body = Body.of_string white in
  let post_req = (Client.post (Uri.of_string "http://localhost:8080/") ~headers:temp_header_with_type
    ~body:temp_body) in
  post_req >>= (fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  return ())

(*get_user_state returns the state of the user*)
(* val get_user_state: uID -> state *)
let get_user_state uID =
  let temp_header = Header.add (Header.init()) "uID" (string_of_int uID) in
  let req = (Client.get (Uri.of_string "http://localhost:8080/") ~headers:temp_header) in
  req >>= (fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  return ())

(*start_heatbeat creates an ivar that represents the heartbeat*)
(* val start_heartbeat: uID -> 'a Async.Std.Ivar.t *)

let start_heartbeat uID =
  let ivar = Ivar.create () in
  ivar

(*check_heartbeat returns false if the heartbeat ever fails*)
(* val check_heartbeat: uID -> boolean *)
let check_heartbeat uID = failwith "unimplemented"
