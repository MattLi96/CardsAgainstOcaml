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

(*create_user_state takes a server state and returns the state of the user*)
val get_user_state: state -> state

(*get a list of active users*)
val get_active_user: unit -> uId list

(*timing*)
(*get_time returns the number of seconds remaining in the round*)
val get_time: unit -> int

(*SET FUNCTIONS: functions that modify the state*)

(*add_user takes in a username and adds it to the list of players in the state*)
val user_add: string -> state

(*remove_user takes in the uID of a player and removes said player from the
list of players in the state*)
val user_remove: uID -> state

(*recognize the fact that a user just has his/her heart beat notified to the
server side, called by server module when a user's heartbeat is received*)
val user_heatbeat: uID -> unit

(*user_play_white adds the card played by a player into the list of played
cards in the state*)
val user_play_white: uID -> card -> state

(*judge_select determines the winner of a round*)
val user_judge: uID -> card -> state

(*reset_all removes all players from the state*)
val game_reset: uID -> state

(*game_start begins the game for all players in the list of players*)
val game_start: unit -> state

(*shuffle takes a state, shuffles the deck, and returns the deck*)
val shuffle: state -> state

