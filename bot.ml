open State
open Client
open Yojson
open Async.Std

(*----------------------Helper Methods---------------------------------*)

(*Precondition: json must be in the form as Austin specified!!!*)
(*Postcondition: return a associative list that maps white card to score*)
let score_of_black json black_card =
  let b_assoc = Yojson.Basic.Util.member black_card json in
  List.map (fun (a,b) -> (a,(b |> Yojson.Basic.Util.to_number)))
    (b_assoc |> Yojson.Basic.Util.to_assoc)

let shuffle_list l =
  let rand_mapped = List.map (fun e -> (Random.bits (), e)) l in
  let sorted = List.sort compare rand_mapped in
  List.map snd sorted

(*Precondition: card_score is a assoc list of (white_card,int), w_list
  is a list of white card, the list cannot be empty*)
let best_card card_score w_list =
  let rec search_max cdl scr lst =
    match lst with
    | [] -> cdl
    | h::t -> let new_scr = List.assoc h card_score in
      if new_scr > scr then search_max [h] new_scr t
      else if new_scr = scr then search_max (h::cdl) scr t
      else search_max cdl scr t in
  let best_lst = search_max [""] min_float w_list in
  Random.self_init ();
  List.hd (shuffle_list best_lst)

let should_play uID (st:univ_c_state) =  print_endline " should play";
  not (List.mem_assoc uID st.played)

let play_card w_card = client_play_white w_card >>=
  (fun _ -> print_endline ("White card played " ^ w_card); return ())

let judge_card w_card = client_judge w_card >>=
  (fun _ -> print_endline ("White card judged " ^ w_card); return ())

(*----------------------Working Methods---------------------------------*)

(*do_game takes a cstate and return a unit defered signifying when the
  action is completed for this do_game (for example: card get played/judged)*)
let do_game json (cstate:c_state): unit Deferred.t =
  try
  (let card_score = score_of_black json (cstate |> get_univ_c).b_card in
  match cstate with
  | Playing st -> if should_play (get_c_uID ()) st then
      let bc = best_card card_score st.hand in
      play_card bc >>= (fun _ -> return ())
    else return ()
  | Judging st -> let bc = best_card card_score (st.played |> List.split |> snd) in
    judge_card bc >>= (fun () -> return ())
  | PWaiting st | JWaiting st -> return ())
  with
  | _ -> print_endline "not working"; return ()

(*start the game form the json read in*)
let rec start_from_json json:unit =
  ignore(client_get_user_state () >>= (do_game json) >>=
  (fun _ -> after (Core.Std.Time.Span.of_sec 1.0) >>=
    (fun _ -> return (start_from_json json))));
  ()

let _ =
  (match Array.to_list Sys.argv with
  | a::b::c::t ->
    ignore(connect_server c "BOT" >>=
    (fun _ -> return (start_from_json (Yojson.Basic.from_file b))))
  | a::b::t ->
    ignore(connect_server "http://localhost:8080/" "BOT" >>=
    (fun _ -> return (start_from_json (Yojson.Basic.from_file b))))
  | _ -> failwith "insufficient argument");
  ignore(Scheduler.go ())
