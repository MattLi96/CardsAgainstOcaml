(*Contains methods for getting various types of information in the game*)
(* module State = sig *)
  type uID
  type white_card
  type black_card
  type deck
  type scores
  type play_state
  type judge_state
  type phase_state

  type state

  (*get_previous_wins returns all of the card pairs, one white and one black,
    that have won previous rounds*)
  val get_previous_wins: state -> (black_card * white_card) list

  (*Method to return the current black card in the state*)
  val curr_black_card: state -> black_card

  (*Method to return scores*)
  val scores: state -> scores

  (*----Play state methods----*)

  (*Method to return the list of users who have played in a given round*)
  val users_played: play_state -> uID list


  (*----Judge state methods----*)

  (*Method to return a list of white cards played*)
  val played_white_cards: judge_state -> white_card list

(* end *)

(*
module type UserState = sig
  (*Module which includes methods for individual users to access state info*)
  include State

  type user_state = {
    state : State.state;
    hand : State.white_card list;
  }

  (*Method for getting a user's hand*)
  val get_hand: user_state -> white_card list

end

module type ServerState = sig
  (*Module which can access all data in the game*)
  include State

  type server_state = {
    state : State.state;
    b_deck : State.deck;
    w_deck : State.deck;

    (*List of (card, player) pairs matching cards played to users who played them*)
    card_to_player : (State.white_card * State.uID) list;
    hands : State.uID * (State.white_card list)
  }

  (*Method for creating an initial server state*)
  val new_state: unit -> server_state

  (*Method for retrieving a user's UserState*)
  val get_user_state: server_state -> uID -> UserState.user_state

  (*Method to return the entire deck*)
  val get_white_deck: state -> white_deck
  val get_black_deck: state -> black_deck

end
*)
