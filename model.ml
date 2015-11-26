(*Model*)
open Timer
open State

(*A model that contains information about the entire game*)
type state = s_state
type white_card = string
type black_card = string
type uID = int

(*GET FUNCTIONS: functions that return information about the state*)

(*get a list of active users*)
(* val get_active_user: unit -> uId list *)
let get_active_user () = failwith "todo"

(*timing*)
(*get_time returns the number of seconds remaining in the round*)
(* val get_time: unit -> int *)
let get_time () = failwith "todo"

(*SET FUNCTIONS: functions that modify the state*)

(*add_user takes in a username and adds it to the list of players in the state*)
(* val user_add: state -> string -> state *)
let user_add state name = failwith "todo"

(*remove_user takes in the uID of a player and removes said player from the
list of players in the state*)
(* val user_remove: state -> uID -> state *)
let user_remove state uID = failwith "todo"

(*recognize the fact that a user just has his/her heart beat notified to the
server side, called by server module when a user's heartbeat is received*)
(* val user_heatbeat: state -> uID -> state *)
let user_heatbeat state uID = failwith "todo"

(*user_play_white adds the card played by a player into the list of played
cards in the state*)
(* val user_play_white: state -> uID -> card -> state *)
let user_play_white state uID white = failwith "todo"

(*judge_select determines the winner of a round*)
(* val user_judge: state -> uID -> card -> state *)
let user_judge state uID white = failwith "todo"

(*reset_all removes all players from the state*)
(* val game_reset: state -> uID -> state *)
let game_reset state uID = failwith "todo"

(*game_start begins the game for all players in the list of players*)
(* val game_start: state -> unit -> state *)
let game_start state () = failwith "todo"

(*shuffle takes a state, shuffles the deck, and returns the deck*)
(* val shuffle: state -> state *)
let shuffle state = failwith "todo"

(*goes to the next game phase*)
(* val game_next_phase: state -> state *)
let game_next_phase state = failwith "todo"



