(*Contains methods for getting various types of information in the game*)

type play_state
type judge_state
type uID
type card
type state = Play of play_state | Judge of judge_state
type timer


(*Method to return the current black card in the state*)
val curr_black_card: state -> card

(*Method to retrieve value of the timer*)
val timer: state -> timer


(*----Play state methods----*)

(*Method to return the list of users who have played in a given round*)
val users_played: play_state -> uID list


(*----Judge state methods----*)
               
(*Method to return a list of white cards played*)
val played_white_cards: judge_state -> card list


