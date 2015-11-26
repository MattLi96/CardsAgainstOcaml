(*Model*)
open State

(*A model that contains information about the entire game*)
type state = s_state
type white_card = string
type black_card = string
type uID = int

(*INIT FUNCTIONS: functions that return information about the state*)
val init_s_state: unit -> state

(*GET FUNCTIONS: functions that return information about the state*)

(*get a list of active users*)
val get_active_user: unit -> uID list

(*SET FUNCTIONS: functions that modify the state*)

(*add_user takes in a username and adds it to the list of players in the state*)
val user_add: state -> string -> (uID * state)

(*remove_user takes in the uID of a player and removes said player from the
list of players in the state*)
val user_remove: state -> uID -> state

(*user_play_white adds the card played by a player into the list of played
cards in the state*)
val user_play_white: state -> uID -> white_card -> state

(*judge_select determines the winner of a round*)
val user_judge: state -> uID -> white_card -> state

(*reset_all removes all players from the state*)
val game_reset: state -> uID -> state

(*game_start begins the game for all players in the list of players*)
val game_start: state -> state

(*shuffle takes a state, shuffles the deck, and returns the deck*)
val shuffle: state -> state

(*goes to the next game phase*)
val game_next_phase: state -> state



