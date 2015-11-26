(*Model*)
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
let user_add state name =
  let new_uID = incr user_counter; !user_counter in
  let new_scores = (new_uID, 0) :: (get_univ_s state).scores in
  let new_card_to_player = (new_uID, None) :: (get_univ_s state).card_to_player in
  let new_hands = (new_uID, []) :: (get_univ_s state).hands in
  let new_state1 = {(get_univ_s state) with scores = new_scores} in
  let new_state2 = {new_state1 with card_to_player = new_card_to_player} in
  let new_state3 = {new_state2 with hands = new_hands} in
  (new_uID, Playing new_state3)

(*remove_user takes in the uID of a player and removes said player from the
list of players in the state*)
(* val user_remove: state -> uID -> state *)
let user_remove state uID = failwith "todo"

let rec modify_card_to_player l uID white =
  match l with
    | [] -> []
    | h::t -> if ((fst h) = uID) then (fst h, Some white)::(modify_card_to_player t uID white) else h::(modify_card_to_player t uID white)

let has_played state uID =
  let rec loop list =
    (match list with
    | [] -> false
    | h::t -> if ((fst h) = uID) then
      (if (snd h <> None) then true else false)
    else
      loop t) in
  loop (get_univ_s state).card_to_player

let rec uID_in_list l uID =
  match l with
    | [] -> false
    | h::t -> if ((fst h) = uID) then true else (uID_in_list t uID)

(*user_play_white adds the card played by a player into the list of played
cards in the state*)
(* val user_play_white: state -> uID -> card -> state *)
let user_play_white (state:state) (uID:uID) (white:white_card):state =
  if (has_played state uID) then
    state
  else
    if (uID_in_list (get_univ_s state).card_to_player uID) then
      let new_played = (uID, white) :: ((get_univ_s state).played) in
      let new_card_to_player = modify_card_to_player (get_univ_s state).card_to_player uID white in
      let new_state1 = {(get_univ_s state) with played = new_played} in
      let new_state2 = {new_state1 with card_to_player = new_card_to_player} in
      Playing new_state2
    else
      state

(*judge_select determines the winner of a round*)
(* val user_judge: state -> uID -> card -> state *)
let user_judge (state:state) (uID:uID) (white:white_card):state =
  match state with
    | Judging _ ->
      if (uID_in_list (get_univ_s state).card_to_player uID) then
        let old_black_card = (get_univ_s state).b_card in
        let new_state = {(get_univ_s state) with winners = Some (old_black_card, white, uID)} in
        Judging new_state
      else
        state
    | _ -> state

(*reset_all removes all players from the state*)
(* val game_reset: state -> uID -> state *)
let game_reset state uID =
  match state with
    | Judging x | Playing x ->
      let new_state = {(get_univ_s (init_s_state ())) with b_card = x.b_card} in
      let new_state2 = {new_state with b_deck = x.b_deck} in
      let new_state3 = {new_state2 with w_deck = x.w_deck} in
      Playing new_state3

(*game_start begins the game for all players in the list of players*)
(* val game_start: state -> unit -> state *)
let game_start state = failwith "todo"

(*shuffle takes a state, shuffles the deck, and returns the deck*)
(* val shuffle: state -> state *)
let shuffle state = failwith "todo"

(*goes to the next game phase*)
(* val game_next_phase: state -> state *)
let game_next_phase state =
  match state with
    | Playing x -> Judging x
    | Judging x -> Playing x


