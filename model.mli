(*Model*)
open State

(*A model that contains information about the entire game*)
type state = s_state
type white_card = string
type black_card = string
type uID = int

(*INIT FUNCTIONS: functions that return information about the state*)
val init_s_state: unit -> state

(*SET FUNCTIONS: functions that modify the state*)

(*add_user takes in a username and adds it to the list of players in the state*)
val user_add: state -> string -> (uID * state)

(*remove_user takes in the uID of a player and removes said player from the
list of players in the state*)
(*Unsupported Functionality*)
val user_remove: state -> uID -> state

(*user_play_white adds the card played by a player into the list of played
cards in the state*)
val user_play_white: state -> uID -> white_card -> state

(*judge_select determines the winner of a round,
  uID must match uID of judge in the state*)
val user_judge: state -> uID -> white_card -> state

(*reset_all removes all players from the state*)
val game_reset: state -> uID -> state

(*game_start begins the game for all players, takes a list of active players*)
val game_start: state -> uID list -> state

(*shuffle takes a state, shuffles the deck, and returns the deck*)
val shuffle: state -> state

(*goes to the next game phase, takes current phase and list of active users*)
val game_next_phase: state -> uID list -> state

(*These functions tell when a state is finished and we can move to next phase*)
val play_state_finished: state -> bool

val judge_state_finished: state -> bool

