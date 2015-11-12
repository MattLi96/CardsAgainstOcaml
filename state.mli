(*Contains methods for getting various types of information in the game*)
module State = sig

  type play_state
  type judge_state
  type uID
  type card = Black of string | White of string
  type state = Play of play_state | Judge of judge_state
  type deck = card list
  type score

  (*get_previous_wins returns all of the card pairs, one white and one black,
    that have won previous rounds*)
  val get_previous_wins: state -> (card * card) list

  (*Method to return the current black card in the state*)
  val curr_black_card: state -> card

  (*Method to return the entire deck*)
  val get_deck: state -> deck

  (*Methods to set the deck to a specific deck*)
  val set_white_deck: state -> deck -> state

  val set_black_deck: state -> deck -> state

  (*Method to return scores*)
  val scores = score list

  (*----Play state methods----*)

  (*Method to return the list of users who have played in a given round*)
  val users_played: play_state -> uID list


  (*----Judge state methods----*)

  (*Method to return a list of white cards played*)
  val played_white_cards: judge_state -> card list

end

module type UserState = sig
  (*Module which includes methods for individual users to access state info*)
  include State

  type user_state

  (*gets the internal state*)
  val get_state: user_state -> state

  (*Method for getting a user's hand*)
  val get_hand: user_state -> card list

end

module type ServerState = sig
  (*Module which can access all data in the game*)
  include State

  type server_state

  (*gets the internal state*) 
  val get_state: server_state -> state

  (*Method for getting any user's hand*)
  val get_user_hand: sever_state -> uID -> card list

  (*Method for setting any user's hand*)
  val deal_single_card: server_state -> uID -> card list

  (*Method for creating an initial server state*)
  val new_state: unit -> server_state

  (*Method for retrieving a user's UserState*)
  val get_user_state: uID -> UserState.user_state
end

