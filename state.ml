  type uID = int
  type white_card = string
  type black_card = string
  type deck = BDeck of black_card list | WDeck of white_card list

  type scores = (uID * int) list

  type univ_c_state = {
    played  : (uID * white_card) list;
    b_card  : black_card;
    scores  : scores;
    winners : (black_card * white_card * uID) list;
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
    winners: (black_card * white_card * uID) list;

    (*decks*)
    b_deck : deck;
    w_deck : deck;

    (*List of (card, player) pairs matching cards played to users who played
    them*)
    card_to_player : (uID * white_card option) list;
    hands          : (uID * (white_card list)) list
  }

  type s_state = Playing of univ_s_state | Judging of univ_s_state

  (*----helper methods------------------------------------------------*)

  let get_univ_c : c_state -> univ_c_state = function
    | Judging s | Playing s | PWaiting s | JWaiting s -> s

  let get_univ_s : s_state -> univ_s_state = function
    | Judging s | Playing s -> s

  (*----client state methods------------------------------------------------*)

  let init_c_state () = failwith "not implemented"

  (*get_previous_wins returns all of the card pairs, one white and one black,
    that have won previous rounds*)
  (* val get_previous_wins: c_state -> (black_card * white_card) list *)
  let get_previous_wins current_state =
    (current_state |> get_univ_c).winners

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
    fst ((current_state |> get_univ_c).played |> List.split)

  (*Method to return a list of white cards played*)
  (* val played_white_cards: c_state -> white_card list *)
  let played_white_cards current_state: white_card list =
    snd ((current_state |> get_univ_c).played |> List.split)

  (*----server state methods------------------------------------------------*)

  let init_s_state () = failwith "not implemented"

  (*Method for retrieving a user's UserState*)
  (* val get_user_state: s_state -> uID -> c_state *)
  let get_user_state (state:s_state) (u:uID):c_state =
    let get_hand state u =
      (let all_hands = (state |> get_univ_s).hands in
      let rec find_match u l_hands =
        (match l_hands with
          | [] -> []
          | h::t ->
            (match h with
              | (uid, cards) ->
                if (uid = u) then cards else find_match u t)
        )
      in
      find_match u all_hands)
    in
    let univ = (state |> get_univ_s) in
    let univ_c = {
      played  = univ.played;
      b_card  = univ.b_card;
      scores  = univ.scores;
      winners = univ.winners;
      hand    = get_hand state u;
    } in
    match state with
    | Playing _ ->
      if (u = (get_univ_s state).judge) then JWaiting univ_c else Playing univ_c
    | Judging _ ->
      if (u = (get_univ_s state).judge) then Judging univ_c else PWaiting univ_c


  (*val get_black_deck: s_state -> deck *)
  let get_black_deck s =
    (s |> get_univ_s).b_deck

  (*Method to return the entire deck*)
  (* val get_white_deck: s_state -> deck*)
  let get_white_deck s =
    (s |> get_univ_s).w_deck

(*----helper methods------------------------------------------------*)

(* val string_winners: (black_card * white_card * uID) list -> string *)
let string_winners winners =
  let rec loop winners =
    (match winners with
      | [] -> ""
      | h::t ->
        (match h with
          | (b,w,u) -> b^"*"^w^"*"^(string_of_int u)^ "|" ^ loop t))
  in
  loop winners

(* val winners_of_string: string -> (black_card * white_card * uID) list *)
let winners_of_string input =
  let rec loop input =
    (match input with
      | "" -> []
      | x ->
        let first_break = String.index x '|' in
        let win = String.sub x 0 first_break in
        let rest = String.sub x (first_break+1) (String.length x -1 - first_break) in
        let cutoff_b = String.index x '*' in
        let black = String.sub win 0 cutoff_b in
        let cutoff_w = String.index_from x (cutoff_b+1) '*' in
        let white = String.sub win (cutoff_b+1) (cutoff_w-1-cutoff_b) in
        let uID = int_of_string(String.sub win (cutoff_w+1) (String.length win - 1 - cutoff_w)) in
        (black, white, uID) :: loop (rest)
      )
  in loop input

(* val string_hand: (white_card list) -> string *)
let string_hand input =
  let rec loop input =
    (match input with
      | [] -> ""
      | h::t -> h ^ "|" ^ (loop t))
  in loop input

(* val hand_of_string: string -> (white_card list) *)
let hand_of_string input =
  let rec loop input =
    (match input with
      | "" -> []
      | x ->
        let first_break = String.index x '|' in
        let card = String.sub x 0 first_break in
        let rest = String.sub x (first_break+1) (String.length x -1 - first_break) in
        card::(loop rest)
      )
  in loop input

(* val string_scores: (uID * int) list -> string *)
let string_scores input =
  let rec loop input =
    (match input with
      | [] -> ""
      | h::t -> (string_of_int (fst h)) ^ "." ^ (string_of_int (snd h)) ^ "|" ^ loop t)
  in
  loop input

(* val scores_of_string: string -> (uID * int) list *)
let scores_of_string input =
  let rec loop input =
    (match input with
      |"" -> []
      | x ->
        let first_break = String.index x '|' in
        let player = String.sub x 0 first_break in
        let rest = String.sub x (first_break+1) (String.length x -1 - first_break) in
        let mid_break = String.index player '.' in
        let uID = int_of_string (String.sub player 0 mid_break) in
        let score = int_of_string (String.sub player (mid_break+1) (String.length player - 1 - mid_break)) in
        (uID, score) :: loop (rest)
      )
  in
  loop input

(* val string_played: (uID * white_card) list -> string *)
let string_played input =
  let rec loop input =
    (match input with
      | [] -> ""
      | h::t -> (string_of_int (fst h)) ^ "." ^ (snd h) ^ "|" ^ loop t)
  in
  loop input

(* val played_of_string: string -> (uID * white_card) list *)
let played_of_string input =
  let rec loop input =
    (match input with
      |"" -> []
      | x ->
        let first_break = String.index x '|' in
        let player = String.sub x 0 first_break in
        let rest = String.sub x (first_break+1) (String.length x -1 - first_break) in
        let mid_break = String.index player '.' in
        let uID = int_of_string (String.sub player 0 mid_break) in
        let wh = (String.sub player (mid_break+1) (String.length player - 1 - mid_break)) in
        (uID, wh) :: loop (rest)
      )
  in
  loop input

  let string_state (cstate:c_state) =
    match cstate with
    | JWaiting _ -> "JWaiting"
    | Judging _ -> "Judging"
    | Playing _ -> "Playing"
    | PWaiting _ -> "PWaiting"

  let state_of_string ustate str =
    match str with
    | "PWaiting" -> PWaiting ustate
    | "Judging" -> Judging ustate
    | "Playing" -> Playing ustate
    | "JWaiting" -> JWaiting ustate
    | _ -> failwith "error in transmission"