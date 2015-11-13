(*Contains methods for getting various types of information in the game*)
module State = sig

  type uID = int
  type card = Black of string | White of string
  type deck = card list
  type scores = (int * int) list (*uID to score*)
  
  type play_state = {played : uID list}
  type judge_state = {played : card list} (*played white cards*)
  type phase_state = Play of play_state | Judge of judge_state
  
  type state = {
    phase : phase_state;
    score : scores;
    winners : (card * card * uID);
    b_card : card;
  }


  (*get_previous_wins returns all of the card pairs, one white and one black,
    that have won previous rounds*)
  val get_previous_wins: state -> (card * card) list

  (*Method to return the current black card in the state*)
  val curr_black_card: state -> card

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

  type user_state = {
    state : State.state;
    hand : State.card list;
  }

  (*Method for getting a user's hand*)
  val get_hand: user_state -> card list

end

module type ServerState = sig
  (*Module which can access all data in the game*)
  include State

  type server_state = {
    state : State.state;
    b_deck : State.deck;
    w_deck : State.deck;

    (*Pairs of played white cards to players*)
    card_to_player : (State.card * State.uID) list; 
    hands : State.uID * (State.card list)
  }

  (*Method for creating an initial server state*)
  val new_state: unit -> server_state

  (*Method for retrieving a user's UserState*)
  val get_user_state: state -> uID -> UserState.user_state

  (*Method to return the entire deck*)
  val get_white_deck: state -> deck
  val get_black_deck: state -> deck

end

