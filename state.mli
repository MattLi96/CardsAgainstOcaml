(*Contains methods for getting various types of information in the game*)
(* module State = sig *)
  type uID = int
  type white_card = string
  type black_card = string
  type deck = BDeck of black_card list | WDeck of white_card list

  (*scores is represented by a list of pairs in which the first element of the
    pair is the uID and the second is the actual score)*)
  type scores = (uID * int) list

  type univ_c_state = {
    played  : (uID * white_card) list;
    b_card  : black_card;
    scores  : scores;
    winners : (black_card * white_card * uID);
    hand    : white_card list;
  }

  type c_state = Judging of univ_c_state
  | Playing of univ_c_state
  | JWaiting of univ_c_state
  | PWaiting of univ_c_state

  type univ_s_state = {
    judge  : uID;
    played : (uID * white_card) list;
    b_card : black_card;
    scores : scores;
    winners: (black_card * white_card * uID);

    (*decks*)
    b_deck : deck;
    w_deck : deck;

    (*List of (card, player) pairs matching cards played to users who played them*)
    card_to_player : (white_card * uID) list;
    hands          : uID * (white_card list)
  }

  type s_state = Playing of univ_s_state | Judging of univ_s_state

  (*----client state methods------------------------------------------------*)

  (*get_previous_wins returns all of the card pairs, one white and one black,
    that have won previous rounds*)
  val get_previous_wins: c_state -> (black_card * white_card) list

  (*Method to return the current black card in the state*)
  val curr_black_card: c_state -> black_card

  (*Method to return scores*)
  val scores: c_state -> scores

  (*Method to return the list of users who have played in a given round*)
  val users_played: c_state -> uID list

  (*Method to return a list of white cards played*)
  val played_white_cards: c_state -> white_card list

  (*----server state methods------------------------------------------------*)

  (*Method for creating an initial server state*)
  val new_state: unit -> s_state

  (*Method for retrieving a user's UserState*)
  val get_user_state: s_state -> uID -> c_state

  (*Method to return the entire deck*)
  val get_white_deck: s_state -> deck
  val get_black_deck: s_state -> deck


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
