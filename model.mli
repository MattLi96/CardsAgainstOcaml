(*Model*)

(*A model that contains information about the entire game*)
type state
type card
type uID

(*GET FUNCTIONS: functions that return information about the state*)
(*get_current_state returns the current state of the game*)
val get_current_state: unit -> state

(*get_previous_wins returns all of the card pairs, one white and one black,
that have won previous rounds*)
val get_previous_wins: unit -> (card * card) list

(*get_hands returns a list of cards in each person's hand*)
val get_hands: (card list) list

(*SET FUNCTIONS: functions that modify the state*)
(*add_user takes in a username and adds it to the list of players in the state*)
val add_user: string -> unit

(*remove_user takes in the uID of a player and removes said player from the list of players in the state*)
val remove_user: uID -> unit

(*reset_all removes all players from the state*)
val reset_all: uID -> unit

(*game_start begins the game for all players in the list of players*)
val game_start: unit -> unit

(*user_play_white adds the card played by a player into the list of played cards in the state*)
val user_play_white: uID -> card -> unit

(*judge_select determines the winner of a round*)
val judge_select: uID -> card -> unit

