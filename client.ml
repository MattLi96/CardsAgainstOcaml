(*Client*)
(*Currently to use: load things in utop to test

#load "state.d.cmo";;
#use "client.ml";;
connect_server !connect_url "austin";;
client_get_user_state ();;
*)

open Async.Std
open Cohttp
open Cohttp_async
open State

type state = c_state

(*Global Variables*)
(*the c_UID that will be generated upon connection*)
let c_uID = ref 0

(*the timer's time for this time*)
let time = ref 40

(*default try to connect to local*)
let connect_url = ref "http://localhost:8080/"

(*Helper Functions*)
let rec get_UID l =
  (match l with
   | [] -> failwith "no uID"
   | h::t -> if (fst h = "uid") then int_of_string (snd h) else get_UID t)

let rec get_param l str =
  (match l with
   | [] -> failwith ("no " ^ str)
   | h::t -> if (fst h = str) then snd h else get_param t str)

(*PUT Requests*)

(*trigger_start sends a signal to the server to start the game*)
let trigger_start () =
  let temp_body = Body.of_string "start" in
  let post_req = (Client.put (Uri.of_string !connect_url) ~body:temp_body) in
  post_req >>= (fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    let ans = (if (code = 200) then
      (Printf.printf ("Game Starting");
      ())
    else
      (Printf.printf "Response code: %d\n" code; ())) in
  return ans)

(*play_white allows a user to play a card*)
(* val play_white: uID -> white_card -> unit *)
let play_white (uID:uID) (white:white_card) =
  let temp_header = Header.add (Header.init()) "uID" (string_of_int uID) in
  let temp_header_with_type = Header.add temp_header "type" "play" in
  let temp_body = Body.of_string white in
  let post_req = (Client.post (Uri.of_string !connect_url)
    ~headers:temp_header_with_type
    ~body:temp_body) in
  post_req >>= (fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  return ())

let client_play_white (white:white_card) =
  play_white !c_uID white

(*judge allows a user to select the winner of a round if he is the judge*)
(* val judge: uID -> white_card -> unit *)
let judge uID white =
  let temp_header = Header.add (Header.init()) "uID" (string_of_int uID) in
  let temp_header_2 = Header.add temp_header "client" (string_of_int !c_uID) in
  let temp_header_with_type = Header.add temp_header_2 "type" "judge" in
  let temp_body = Body.of_string white in
  let post_req = (Client.post (Uri.of_string !connect_url)
    ~headers:temp_header_with_type
    ~body:temp_body) in
  post_req >>= (fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  return ())

let client_judge (white:white_card) =
  judge !c_uID white

(*pausing and resuming the game*)
let client_pause () =
  let temp_header = Header.add (Header.init()) "uID" (string_of_int !c_uID) in
  let temp_header_with_type = Header.add temp_header "type" "pause" in
  let post_req = (Client.post (Uri.of_string !connect_url)
    ~headers:temp_header_with_type
    ~body:(Body.of_string "")) in
  post_req >>= (fun (resp,body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  return ())

let client_resume () =
  let temp_header = Header.add (Header.init()) "uID" (string_of_int !c_uID) in
  let temp_header_with_type = Header.add temp_header "type" "resume" in
  let post_req = (Client.post (Uri.of_string !connect_url)
    ~headers:temp_header_with_type
    ~body:(Body.of_string "")) in
  post_req >>= (fun (resp,body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  return ())

let client_beat () =
  let temp_header = Header.add (Header.init()) "uID" (string_of_int !c_uID) in
  let temp_header_with_type = Header.add temp_header "type" "beat" in
  let post_req = (Client.post (Uri.of_string !connect_url)
    ~headers:temp_header_with_type
    ~body:(Body.of_string "")) in
  post_req >>= (fun (resp,body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  time := (get_param (resp |> Response.headers |> Header.to_list) "time") |> int_of_string;
  return ())


(*start_heatbeat creates an ivar that represents the heartbeat*)
(* val start_heartbeat: uID -> 'a Async.Std.Ivar.t *)
let start_heartbeat () =
  let rec beat () =
    ignore(client_beat ());
    let _ = after (Core.Std.Time.Span.of_sec 0.1) >>=
      (fun _ -> beat (); print_endline (!time |> string_of_int); return ()) in
  () in
  beat ()

  (*connect_server allows the user to connect to a server at a given url
  with the username of their choice*)
let connect_server url name =
  connect_url := url;
  let temp_header = Header.add (Header.init()) "name" (name) in
  let post_req = (Client.put (Uri.of_string !connect_url)
                  ~headers:temp_header) in
  post_req >>= (fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    let ans = (if (code = 200) then
      (Printf.printf ("Connection successful");
      start_heartbeat ();
      (c_uID := get_UID (resp |> Response.headers |> Header.to_list));
      ())
    else
      (Printf.printf "Response code: %d\n" code; ())) in
  return ans)

(*get_user_state returns the state of the user*)
(* val get_user_state: uID -> state *)
let get_user_state (uID:uID):state Deferred.t =
  let temp_header = Header.add (Header.init()) "uID" (string_of_int uID) in
  let req = (Client.get (Uri.of_string !connect_url) ~headers:temp_header) in
  req >>= (fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  let response_h = (resp |> Response.headers |> Header.to_list) in

  let played = played_of_string (get_param response_h "played") in
  let b_card = (get_param response_h "b_card") in
  let scores = scores_of_string (get_param response_h "scores") in
  let winners = winners_of_string (get_param response_h "winners") in
  let hand = hand_of_string (get_param response_h "hand") in
  let str_state = get_param response_h "state" in
  let ans = {
    played  = played;
    b_card  = b_card;
    scores  = scores;
    winners = winners;
    hand    = hand;
  } in
  let ans2 = State.state_of_string ans str_state in
  return ans2)

let client_get_user_state () =
  get_user_state !c_uID

(*start_heatbeat creates an ivar that represents the heartbeat*)
(* val start_heartbeat: uID -> 'a Async.Std.Ivar.t *)
let start_heartbeat uID =
  let ivar = Ivar.create () in
  ivar

let current_id () =
  !c_uID

(*check_heartbeat returns false if the heartbeat ever fails*)
(* val check_heartbeat: uID -> boolean *)
let check_heartbeat uID = failwith "unimplemented"
