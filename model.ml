(*Model*)
open Timer
open State

(*A model that contains information about the entire game*)
type state = s_state
type white_card = string
type black_card = string
type uID = int

let user_counter = ref 0

(*INIT FUNCTIONS*)
let init_s_state () =
  let temp_state = {
    judge  = 0;
    played = [];
    b_card = "";
    scores = [];
    winners = None;

    (*decks*)
    (*CHANGE TO IMPORT DECKS FROM JSON*)
    b_deck = BDeck [];
    w_deck = WDeck [];

    (*List of (card, player) pairs matching cards played to users who played
    them*)
    card_to_player = [];
    hands          = []
  } in
  Playing temp_state

(*GET FUNCTIONS: functions that return information about the state*)

(*get a list of active users*)
(* val get_active_user: unit -> uId list *)
let get_active_user () = failwith "todo"

(*SET FUNCTIONS: functions that modify the state*)

(*add_user takes in a username and adds it to the list of players in the state*)
(* val user_add: state -> string -> state *)
let user_add state name = failwith "todo"

(*remove_user takes in the uID of a player and removes said player from the
list of players in the state*)
(* val user_remove: state -> uID -> state *)
let user_remove state uID = failwith "todo"

let has_played state uID =
  let rec loop list =
    (match list with
    | [] -> failwith "player doesn't exist"
    | h::t -> if ((fst h) = uID) then
      (if (snd h <> None) then true else false)
    else
      loop t) in
  loop (get_univ_s state).card_to_player

(*user_play_white adds the card played by a player into the list of played
cards in the state*)
(* val user_play_white: state -> uID -> card -> state *)
let user_play_white (state:state) (uID:uID) (white:white_card):state =
  if (has_played state uID) then
    state
  else
    failwith "todo"

(*judge_select determines the winner of a round*)
(* val user_judge: state -> uID -> card -> state *)
let user_judge (state:state) (uID:uID) (white:white_card):state =
  let old_black_card = (get_univ_s state).b_card in
  let new_state = {(get_univ_s state) with winners = Some (old_black_card, white, uID)} in
  Judging new_state

(*reset_all removes all players from the state*)
(* val game_reset: state -> uID -> state *)
let game_reset state uID = failwith "todo"

(*game_start begins the game for all players in the list of players*)
(* val game_start: state -> unit -> state *)
let game_start state () = failwith "todo"

(*shuffle takes a state, shuffles the deck, and returns the deck*)
(* val shuffle: state -> state *)
let shuffle state = failwith "todo"

(*goes to the next game phase*)
(* val game_next_phase: state -> state *)
let game_next_phase state = failwith "todo"



