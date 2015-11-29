(*Model*)
open State
open Yojson.Basic.Util

(*A model that contains information about the entire game*)
type state = s_state
type white_card = string
type black_card = string
type uID = int

let user_counter = ref 0
let req_num_of_cards = 10

let fill_deck file_name =
  let json = Yojson.Basic.from_file file_name in
  let cards = json |> to_list in
  List.map (fun json -> member "text" json |> to_string) cards

let transfer l num =
  let rec loop num ans =
    (if (num = 0) then ans
    else
      (match ans with
        | (from, dest) ->
          (match from with
            | [] -> ans
            | h::t -> loop (num -1) (t, h::dest))))
  in
  loop num (l, [])

let give_cards (u_s_state:univ_s_state):univ_s_state =
  let deck = ref (match (u_s_state.w_deck) with
    | WDeck x -> x
    | BDeck x -> []) in
  let rec loop d l =
    (match l with
      | [] -> []
      | h::t ->
        let current_num = List.length (snd h) in
        let dif = req_num_of_cards - current_num in
        let trans = transfer !d dif in
        d := (fst trans);
        (fst h, (snd h) @ (snd trans)) :: (loop (d) t)) in
  let new_hands = loop deck u_s_state.hands in
  let new_state = {u_s_state with hands = new_hands} in
  {new_state with w_deck = WDeck !deck}

(* (uID * (white_card list)) list *)

let cycle_judge (card_to_player: (uID * white_card option) list) (current_judge: uID) =
  let new_list = card_to_player @ card_to_player in
  let rec loop l id =
    (match l with
      | [] -> [(0, None)]
      | h::t -> if (fst h = id) then t else loop t id) in
  let part = loop new_list current_judge in
  fst (List.hd part)


let select_black (u_s_state:univ_s_state):univ_s_state =
  let old_b_deck = (match u_s_state.b_deck with
    | BDeck x -> x
    | WDeck _ -> []) in
  let new_b = (match old_b_deck with
    | [] -> ""
    | h::t -> h) in
  let new_d = (match old_b_deck with
    | [] -> []
    | h::t -> t) in
  let new_state = {
    judge  = cycle_judge u_s_state.card_to_player u_s_state.judge;
    played = [];
    b_card = new_b;
    scores = u_s_state.scores;
    winners = u_s_state.winners;

    b_deck = BDeck new_d;
    w_deck = u_s_state.w_deck;

    card_to_player = List.map (fun (uid, card) -> (uid, None))
                     u_s_state.card_to_player;
    hands          = u_s_state.hands
  } in
  new_state

(*INIT FUNCTIONS*)
let init_s_state () =
  let temp_state = {
    judge  = 0;
    played = [];
    b_card = "";
    scores = [];
    winners = [];

    (*decks*)
    b_deck = BDeck (fill_deck "black.json");
    w_deck = WDeck (fill_deck "white.json");

    (*List of (card, player) pairs matching cards played to users who played
    them*)
    card_to_player = [];
    hands          = []
  } in
  let sel_blck = select_black temp_state in
  let card_distro = give_cards sel_blck in
  Playing card_distro

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
  let new_card_to_player = (new_uID, None) ::
                           (get_univ_s state).card_to_player in
  let new_hands = (new_uID, []) :: (get_univ_s state).hands in
  let new_state1 = {(get_univ_s state) with scores = new_scores} in
  let new_state2 = {new_state1 with card_to_player = new_card_to_player} in
  let new_state3 = {new_state2 with hands = new_hands} in
  let final_state = give_cards new_state3 in
  (new_uID, Playing final_state)

(*remove_user takes in the uID of a player and removes said player from the
list of players in the state*)
(* val user_remove: state -> uID -> state *)
let user_remove state uID = failwith "todo"

let rec modify_card_to_player l uID white =
  match l with
    | [] -> []
    | h::t -> if ((fst h) = uID) then
        (fst h, Some white)::(modify_card_to_player t uID white)
      else
        h::(modify_card_to_player t uID white)

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

let remove_card_from_hand hands uID white =
  let sel_fun = (fun (u, l) ->
    if (u=uID) then
      let rec remove hand white =
        (match hand with
          | [] -> []
          | h::t ->
            (if (h <> white) then
              h::(remove t white)
            else
              (remove t white)))
      in (u, remove l white)
    else
      (u, l)) in
  List.map (sel_fun) hands

(*user_play_white adds the card played by a player into the list of played
cards in the state*)
(* val user_play_white: state -> uID -> card -> state *)
let user_play_white (state:state) (uID:uID) (white:white_card):state =
  if (has_played state uID) then
    state
  else
    if (uID_in_list (get_univ_s state).card_to_player uID) then
      let old_hands = (get_univ_s state).hands in
      let new_hands = (remove_card_from_hand old_hands uID white) in
      let new_played = (uID, white) :: ((get_univ_s state).played) in
      let new_card_to_player = modify_card_to_player
                                (get_univ_s state).card_to_player uID white in
      let new_state1 = {(get_univ_s state) with played = new_played} in
      let new_state2 = {new_state1 with card_to_player = new_card_to_player} in
      let new_state3 = {new_state2 with hands = new_hands} in
      Playing new_state3
    else
      state

(* type scores = (uID * int) list *)
let give_point scores uID =
  let sel_fun = (fun (u, s) ->
    if (u=uID) then (u, s+1) else (u,s) ) in
  List.map sel_fun scores

(*judge_select determines the winner of a round*)
(* val user_judge: state -> uID -> card -> state *)
let user_judge (state:state) (uID:uID) (white:white_card):state =
  match state with
    | Judging _ ->
      if (uID_in_list (get_univ_s state).card_to_player uID) then
        let old_black_card = (get_univ_s state).b_card in
        let new_scores = give_point (get_univ_s state).scores uID in
        let new_state = {(get_univ_s state) with winners =
          (old_black_card, white, uID) :: (get_univ_s state).winners} in
        let new_state2 = {new_state with scores = new_scores} in
        Judging new_state2
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

(*Shuffle helpers*)
(*borrowed shuffle_list from google*)
let shuffle_list l =
  let rand_mapped = List.map (fun e -> (Random.bits (), e)) l in
  let sorted = List.sort compare rand_mapped in
  List.map snd sorted

let rec shuffle_help deck =
  match deck with
  | BDeck d -> BDeck (shuffle_list d)
  | WDeck d -> WDeck (shuffle_list d)

(*shuffle takes a state, shuffles the deck, and returns the deck*)
(* val shuffle: state -> state *)
let shuffle state =
  match state with
  | Judging x ->
    Judging {x with b_deck = shuffle_help x.b_deck;
                    w_deck = shuffle_help x.w_deck}
  | Playing x ->
    Playing {x with b_deck = shuffle_help x.b_deck;
                    w_deck = shuffle_help x.w_deck}

(*game_start begins the game for all players in the list of players*)
(* val game_start: state -> state *)
let game_start state =
  let s = shuffle state in
  match s with
  | Playing x | Judging x -> Playing x

(*goes to the next game phase*)
(* val game_next_phase: state -> state *)
let game_next_phase state =
  match state with
  | Playing x -> Judging x
  | Judging x -> Playing (select_black x)

let play_state_finished state =
  match state with
  | Judging x -> false
  | Playing x ->
    not (List.exists (fun p -> (snd p) = None) (get_univ_s state).card_to_player)

let judge_state_finished state =
  match state with
  | Playing x -> false
  | Judging x ->
    if ((get_univ_s state).winners = []) then false else
    let first_el = List.hd (get_univ_s state).winners in
    (match (first_el) with
      | (b, w, u) -> b = (get_univ_s state).b_card)