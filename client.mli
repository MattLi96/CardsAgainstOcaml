(*Client*)
open State

type state

(*play_white allows a user to play a card*)
val play_white: uID -> white_card -> unit

(*judge allows a user to select the winner of a round if he is the judge*)
val judge: uID -> white_card -> unit

(*get_user_state returns the state of the user*)
val get_user_state: uID -> state

(*start_heatbeat creates an ivar that represents the heartbeat*)
val start_heartbeat: uID -> 'a Async.Std.Ivar.t

(*check_heartbeat returns false if the heartbeat ever fails*)
val check_heartbeat: uID -> bool
