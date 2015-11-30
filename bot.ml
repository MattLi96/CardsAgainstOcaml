open State
open Client
open Yojson
open Async.Std

let play_card w_card = client_play_white w_card >>=
  (fun _ -> print_endline ("White card played" ^ w_card); return ())

let judge_card w_card = client_judge w_card >>=
  (fun _ -> print_endline ("White card judged" ^ w_card); return ())

(*do_game takes a cstate and return a unit defered signifying when the
  action is completed for this do_game (for example: card get played/judged)*)
let do_game (cstate:c_state): unit Deferred.t =
  match cstate with
  | Playing st -> failwith "unimplemented"
  | Judging st -> failwith "unimplemented"
  | PWaiting st -> failwith "unimplemented"
  | JWaiting st -> failwith "unimplemented"

(*start the game form the json read in*)
let rec start_from_json json:unit =
  ignore(client_get_user_state () >>= do_game >>=
  (fun _ -> after (Core.Std.Time.Span.of_sec 1.0) >>=
    (fun _ -> return (start_from_json json))));
  ()

let _ =
  (match Array.to_list Sys.argv with
  | a::b::t -> start_from_json (Yojson.Basic.from_file b)
  | _ -> failwith "insufficient argument");
  ignore(Scheduler.go ())
