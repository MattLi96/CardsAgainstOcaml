(*Client*)
open Async.Std
open Cohttp
open State

type state = c_state

(*play_white allows a user to play a card*)
(* val play_white: uID -> white_card -> unit *)

let play_white uID white = failwith "unimplemented"

(*judge allows a user to select the winner of a round if he is the judge*)
(* val judge: uID -> white_card -> unit *)

let judge uID white = failwith "unimplemented"

(*get_user_state returns the state of the user*)
(* val get_user_state: uID -> state *)

let get_user_state uID = failwith "unimplemented"

(*start_heatbeat creates an ivar that represents the heartbeat*)
(* val start_heartbeat: uID -> 'a Async.Std.Ivar.t *)

let start_heartbeat uID = failwith "unimplemented"

(*check_heartbeat returns false if the heartbeat ever fails*)
(* val check_heartbeat: uID -> boolean *)

let check_heartbeat uID = failwith "unimplemented"
