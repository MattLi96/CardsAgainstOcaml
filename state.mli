(*Contains methods for getting various types of information in the game*)
module State : sig

  type play_state
  type judge_state
  type uID
  type card
  type state = Play of play_state | Judge of judge_state
  type deck = card list


  (*Method to return the current black card in the state*)
  val curr_black_card: state -> card

  (*Method to return the entire deck*)
  val get_deck: state -> deck

  (*Methods to set the deck to a specific deck*)
  val set_white_deck: state -> deck -> state

  val set_black_deck: state -> deck -> state
  (*----Play state methods----*)

  (*Method to return the list of users who have played in a given round*)
  val users_played: play_state -> uID list


  (*----Judge state methods----*)

  (*Method to return a list of white cards played*)
  val played_white_cards: judge_state -> card list

end


