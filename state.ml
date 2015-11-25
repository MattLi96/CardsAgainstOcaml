  type uID = int
  type white_card = string
  type black_card = string
  type deck = BDeck of black_card list | WDeck of white_card list

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

    (*List of (card, player) pairs matching cards played to users who played
    them*)
    card_to_player : (white_card * uID) list;
    hands          : (uID * (white_card list)) list
  }

  type s_state = Playing of univ_s_state | Judging of univ_s_state

  (*----helper methods------------------------------------------------*)

  let get_univ_c : c_state -> univ_c_state = function
    | Judging s | Playing s | PWaiting s | JWaiting s -> s

  let get_univ_s : s_state -> univ_s_state = function
    | Judging s | Playing s -> s

  (*----client state methods------------------------------------------------*)

  (*get_previous_wins returns all of the card pairs, one white and one black,
    that have won previous rounds*)
  (* val get_previous_wins: c_state -> (black_card * white_card) list *)
  let get_previous_wins current_state =
    failwith "to implement"

  (*Method to return the current black card in the state*)
  (* val curr_black_card: c_state -> black_card *)
  let curr_black_card current_state =
    (current_state |> get_univ_c).b_card

  (*Method to return scores*)
  (* val scores: c_state -> scores *)
  let scores current_state =
    (current_state |> get_univ_c).scores

  (*Method to return the list of users who have played in a given round*)
  (* val users_played: c_state -> uID list *)
  let users_played current_state: uID list =
    let played = (current_state |> get_univ_c).played in
    let rec extract_uID input =
      (match input with
        | [] -> []
        | h::t ->
          (match h with
            | (uid, card) -> uid :: (extract_uID t)))
    in
    extract_uID played

  (*Method to return a list of white cards played*)
  (* val played_white_cards: c_state -> white_card list *)
  let played_white_cards current_state: white_card list =
    let played = (current_state |> get_univ_c).played in
    let rec extract_card input =
      (match input with
        | [] -> []
        | h::t ->
          (match h with
            | (uid, card) -> card :: (extract_card t)))
    in
    extract_card played

  (*----server state methods------------------------------------------------*)

  (*Method for retrieving a user's UserState*)
  (* val get_user_state: s_state -> uID -> c_state *)
  let get_user_state state u =
    (* let all_hands = (state |> get_univ_s).hands in
    let rec find_match u l_hands =
      (match l_hands with
        | [] -> None
        | h::t ->
          (match h with
            | (uid, cards) ->
              if (uid = u) then Some cards else find_match u t)
      )
    in
    find_match u all_hands *)
    failwith "unimplemented"

  (*Method for creating an initial server state*)
  (* val new_state: unit -> s_state *)
  let new_state () = failwith "unimplemented"

  (*val get_black_deck: s_state -> deck *)
  let get_black_deck s =
    (s |> get_univ_s).b_deck

  (*Method to return the entire deck*)
  (* val get_white_deck: s_state -> deck*)
  let get_white_deck s =
    (s |> get_univ_s).w_deck
