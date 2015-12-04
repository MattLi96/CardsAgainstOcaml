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

(*fills hands up with white cards*)
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
       d := (fst trans) @ (snd trans); (*cycle cards to the back*)
       (fst h, (snd h) @ (snd trans)) :: (loop (d) t)) in
  let new_hands = loop deck u_s_state.hands in
  let new_state = {u_s_state with hands = new_hands} in
  {new_state with w_deck = WDeck !deck}

let cycle_judge (card_to_player: (uID * white_card option) list) (current_judge: uID) =
  let rec loop l id =
    (match l with
     | [] | h::[] -> card_to_player
     | h::t -> if (fst h = id || id = 0) then t else loop t id) in
  match loop card_to_player current_judge with
  | [] -> 0 (*No valid judges avalible*)
  | h::t -> fst h

let select_black (u_s_state:univ_s_state) : univ_s_state =
  let old_b_deck = (
    match u_s_state.b_deck with
    | BDeck x -> x
    | WDeck _ -> []) in
  let (new_b, new_d) = (
    match old_b_deck with
    | [] -> ("", [])
    | h::t -> (h, t @ [h])) in
  {u_s_state with
   played = [];
   b_card = new_b;
   b_deck = BDeck new_d;
  }

(*INIT FUNCTIONS*)
let init_s_state () =
  match State.init_s_state () with
  | Playing x | Judging x ->
    Playing {x with
             (*decks*)
             b_deck = BDeck (fill_deck "black.json");
             w_deck = WDeck (fill_deck "white.json");
            }

(*SET FUNCTIONS: functions that modify the state*)
(*univ_s is a univ_s_state. Rest are the same*)
let user_add_helper univ_s name =
  let new_uID = incr user_counter; !user_counter in
  let new_scores = (new_uID, 0) :: univ_s.scores in
  let new_card_to_player = (new_uID, None) ::
                           univ_s.card_to_player in
  let new_hands = (new_uID, [])::univ_s.hands in
  let final_state = {univ_s with
                    scores = new_scores;
                    card_to_player = new_card_to_player;
                    hands = new_hands
                   } in
  (new_uID, final_state)

(*add_user takes in a username and adds it to the list of players in the state*)
(* val user_add: state -> string -> state *)
let user_add state name =
  let res = user_add_helper (get_univ_s state) name in
  match (state, res) with
  | (Playing _, (uid, s)) -> (uid, Playing s)
  | (Judging _, (uid, s)) -> (uid, Judging s)

(*remove_user takes in the uID of a player and removes said player from the
  list of players in the state*)
(*PRECONDITION: the single last player cannot be removed*)
(* val user_remove: state -> uID -> state *)
let user_remove state uID =
  let ustate = get_univ_s state in
  let new_ustate_draft =
  {ustate with hands = List.remove_assoc uID ustate.hands;
              scores = List.remove_assoc uID ustate.scores;
              played = List.remove_assoc uID ustate.played;
      card_to_player = List.remove_assoc uID ustate.card_to_player
  } in
  let new_ustate = if ustate.judge = uID then (*The judge is removed*)
    let new_j = cycle_judge ustate.card_to_player ustate.judge in
    if new_j = uID then state |> get_univ_s
    else {new_ustate_draft with judge = new_j}
  else new_ustate_draft in
  match state with
  | Playing _ -> Playing new_ustate
  | Judging _ -> Judging new_ustate


let modify_card_to_player l uID white =
  List.map (fun (id, op) -> if id = uID then (id, Some white) else (id, op)) l

let remove_card_from_hand hands uID white =
  let sel_fun = (fun (u, l) ->
      if (u=uID) then
        (u, List.filter (fun x -> x <> white) l)
      else
        (u, l))
  in
  List.map (sel_fun) hands

(*user_play_white adds the card played by a player into the list of played
  cards in the state*)
(* val user_play_white: state -> uID -> card -> state *)
let user_play_white (state:state) (uID:uID) (white:white_card):state =
  match state with
  | Playing x ->
    if List.exists (fun (id, op) -> id = uID && op <> None) x.card_to_player
    then state
    else if List.exists (fun (id, _) -> id = uID) x.card_to_player then
      let new_hands = (remove_card_from_hand x.hands uID white) in
      let new_played = (uID, white)::x.played in
      let new_card_to_player = modify_card_to_player x.card_to_player uID white in
      let new_state = {x with
                        played = new_played;
                        card_to_player = new_card_to_player;
                        hands = new_hands
                       } in
      Playing new_state
    else
      state
  | Judging x -> state

(* type scores = (uID * int) list *)
let give_point scores uID =
  let sel_fun = (fun (u, s) ->
      if (u=uID) then (u, s+1) else (u,s) ) in
  List.map sel_fun scores

(*judge_select determines the winner of a round*)
(* val user_judge: state -> uID -> card -> state *)
let user_judge (state:state) (uID:uID) (white:white_card) : state =
  match state with
  | Playing x -> state
  | Judging x ->
    if (white = "") || (x.judge <> uID) then state
    else
      let win = List.filter (fun (_, card) -> card = white) x.played in
      Judging
        (List.fold_left
           (fun x2 (wID, _) ->
              let old_black_card = x2.b_card in
              let new_scores = give_point x2.scores wID in
              let new_state = {x2 with
                               winners = (old_black_card, white, wID)::x2.winners} in
              let new_state2 = {new_state with scores = new_scores} in
              new_state2
           )
           x win
        )

(*reset_all removes all players from the state*)
(* val game_reset: state -> uID -> state *)
let game_reset state uID =
  match state with
  | Judging x | Playing x ->
    let new_state = {(get_univ_s (init_s_state ())) with
                     b_deck = x.b_deck;
                     w_deck = x.w_deck
                    } in
    Playing new_state

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
  Random.self_init ();
  let s = shuffle state in
  match s with
  | Judging x | Playing x ->
    let sel_blck = select_black x in
    let card_distro = give_cards sel_blck in
    Playing card_distro

(*goes to the next game phase*)
(* val game_next_phase: state -> uID list -> state *)
let rec game_next_phase state active_users =
  match state with
  | Playing x ->
    if (List.exists (fun p -> (snd p) <> None) x.card_to_player)
    then Judging x
    else game_next_phase (Judging x) active_users
  | Judging x ->
    (*Implement player removal and judge cycling here*)
    let new_judge = cycle_judge x.card_to_player x.judge in

    let new_hands_state = give_cards {x with judge = new_judge} in
    let new_black_state = select_black new_hands_state in
    Playing {new_black_state with
             card_to_player = List.map (fun (uid, _) -> (uid, None))
                 new_black_state.card_to_player;
            }

let play_state_finished state =
  match state with
  | Judging x -> false
  | Playing x ->
    (List.length (List.filter (fun p -> (snd p) = None) x.card_to_player) = 1)

let judge_state_finished state =
  match state with
  | Playing x -> false
  | Judging x ->
    if (x.winners = []) then false else
      let first_el = List.hd x.winners in
      (match (first_el) with
       | (b, _, _) -> b = x.b_card)
