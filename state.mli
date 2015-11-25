(*Contains methods for getting various types of information in the game*)
(* module State = sig *)
  type uID
  type white_card
  type black_card
  type deck

  (*scores is represented by a list of pairs in which the first element of the
    pair is the uID and the second is the actual score)*)
  type scores

  (*c_state refers to the state of the client and contains all fields that
  the client can access*)
  type univ_c_state
  type c_state

  (*s_state refers to the state of the server and contains all fields that the
  server can access*)
  type univ_s_state
  type s_state

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
  (*Method for retrieving a user's UserState*)
  val get_user_state: s_state -> uID -> c_state option

  (*Method for creating an initial server state*)
  val new_state: unit -> s_state

  (*Method to return the entire deck*)
  val get_white_deck: s_state -> deck
  val get_black_deck: s_state -> deck