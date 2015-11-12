(*Model*)
open UserState

(*A model that contains information about the entire game*)
type state = server_state
type card
type uID

(*GET FUNCTIONS: functions that return information about the state*)

(*get a list of active users*)
val get_active_user: unit -> uId list

(*timing*)
(*get_time returns the number of seconds remaining in the round*)
val get_time: unit -> int

(*SET FUNCTIONS: functions that modify the state*)

(*add_user takes in a username and adds it to the list of players in the state*)
val user_add: state -> string -> state

(*remove_user takes in the uID of a player and removes said player from the
list of players in the state*)
val user_remove: state -> uID -> state

(*recognize the fact that a user just has his/her heart beat notified to the
server side, called by server module when a user's heartbeat is received*)
val user_heatbeat: state -> uID -> state

(*user_play_white adds the card played by a player into the list of played
cards in the state*)
val user_play_white: state -> uID -> card -> state

(*judge_select determines the winner of a round*)
val user_judge: state -> uID -> card -> state

(*reset_all removes all players from the state*)
val game_reset: state -> uID -> state

(*game_start begins the game for all players in the list of players*)
val game_start: state -> unit -> state

(*shuffle takes a state, shuffles the deck, and returns the deck*)
val shuffle: state -> state

(*goes to the next game phase*)
val game_next_phase: state -> state



