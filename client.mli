(*Client*)
open Async.Std
open State

type state = c_state

val time: int ref

val c_uID: int ref

(*connect_server allows the client to connect to a server*)
val connect_server: string -> string -> unit Deferred.t

val client_play_white: white_card -> unit Deferred.t
val client_judge: white_card -> unit Deferred.t
val client_get_user_state: unit -> state Deferred.t

(*pause the game*)
val client_pause: unit -> unit Deferred.t

(*resume the game(start game timer)*)
val client_resume: unit -> unit Deferred.t

(*play_white allows a user to play a card*)
val play_white: uID -> white_card -> unit Deferred.t

(*judge allows a user to select the winner of a round if he is the judge*)
val judge: uID -> white_card -> unit Deferred.t

(*get_user_state returns the state of the user*)
val get_user_state: uID -> state Deferred.t

(*check_heartbeat returns false if the heartbeat ever fails*)
val check_heartbeat: uID -> bool

val trigger_start: unit -> unit Deferred.t
