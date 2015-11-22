  type uID = int
  type white_card = string
  type black_card = string
  type deck = BDeck of black_card list | WDeck of white_card list
  (*scores is represented by a list of pairs in which the first element of the
    pair is the uID and the second is the actual score)*)
  type scores = (uID * int) list

  type play_state = {played : uID list}
  type judge_state = {played : black_card list} (*played white cards*)
  type phase_state = Play of play_state | Judge of judge_state

  type state = {
    phase : phase_state;
    score : scores;
    winners : (black_card * white_card * uID);
    b_card : black_card;
  }

  (*get_previous_wins returns all of the card pairs, one white and one black,
    that have won previous rounds*)
  (* val get_previous_wins: state -> (black_card * white_card) list *)

  let get_previous_wins current_state =
    failwith "to implement"

  (*Method to return the current black card in the state*)
  (* val curr_black_card: state -> black_card *)

  let curr_black_card current_state =
    current_state.b_card

  (*Method to return scores*)
  (* val scores: state -> scores *)

  let scores current_state =
    failwith "to implement"

  (*----Play state methods----*)

  (*Method to return the list of users who have played in a given round*)
  (* val users_played: play_state -> uID list *)

  let users_played current_play_state =
    failwith "to implement"

  (*----Judge state methods----*)

  (*Method to return a list of white cards played*)
  (* val played_white_cards: judge_state -> white_card list *)

  let played_white_cards current_judge_state =
    failwith "to implement"