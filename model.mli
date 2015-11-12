(*Model*)

(*A model that contains information about the entire game*)
type state
type card = Black of string | White of string
type uID

(*GET FUNCTIONS: functions that return information about the state*)
(*get_current_state returns the current state of the game*)
val get_current_state: unit -> state

(*get_previous_wins returns all of the card pairs, one white and one black,
that have won previous rounds*)
val get_previous_wins: unit -> (card * card) list

(*get_hands returns a list of cards in each person's hand*)
val get_hands: state -> (card list) list

(*SET FUNCTIONS: functions that modify the state*)
(*add_user takes in a username and adds it to the list of players in the state*)
val add_user: string -> state

(*remove_user takes in the uID of a player and removes said player from the
list of players in the state*)
val remove_user: uID -> state

(*reset_all removes all players from the state*)
val reset_all: uID -> state

(*game_start begins the game for all players in the list of players*)
val game_start: unit -> state

(*user_play_white adds the card played by a player into the list of played
cards in the state*)
val user_play_white: uID -> card -> state

(*judge_select determines the winner of a round*)
val judge_select: uID -> card -> state

(*shuffle takes a state, shuffles the deck, and returns the deck*)
val shuffle: state -> state

(*create_user_state takes a server state and returns the state of the user*)
val create_user_state: state -> state