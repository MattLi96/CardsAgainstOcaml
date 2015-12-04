open State
open Client
open Yojson
open Async.Std
open RecurringCall
open Timer

(*-----------GUI-------------*)

let locale () = GtkMain.Main.init ()
let destroy () = GMain.Main.quit ()


let player_hand = ref None
let submissions = ref None
let gamelog = GText.buffer ()
let winner_log = ref None
let (curr_user_state: State.univ_c_state option ref) = ref None
let (trainer_json: Yojson.Basic.json option ref) = ref None

(*----------------------Helper Methods---------------------------------*)

(*Precondition: json must be in the form as Austin specified!!!*)
(*Postcondition: return a associative list that maps white card to score*)
let score_of_black json black_card =
  let b_assoc = Yojson.Basic.Util.member black_card json in
  List.map (fun (a,b) -> (a,(b |> Yojson.Basic.Util.to_int)))
    (b_assoc |> Yojson.Basic.Util.to_assoc)

(*Precondition: card_score is a assoc list of (white_card,int), w_list
  is a list of white card*)
let best_card card_score w_list =
  let rec search_max cd scr lst =
    match lst with
    | [] -> cd
    | h::t -> let new_scr = List.assoc h card_score in
      if new_scr > scr then search_max h new_scr t
      else search_max cd scr t in
  search_max "" min_int w_list

let should_play uID (st:univ_c_state) = not (List.mem_assoc uID st.played)

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
  | _ -> return ()

(*start the game form the json read in*)
let start_from_json () :unit =
  upon (client_get_user_state ()) (fun x -> 
      match !trainer_json with
      | None -> ()
      | Some b -> ignore (do_game b x); ())


(*GUI CODE*)
module FormatOps = struct

  type cardsfile = Yojson.Basic.json

  let rec wordlist (x:string) =
    if String.length x = 0 then [] else
    if String.contains x ' ' then
      let currindex = (String.index x ' ') in
      let remainingstring = String.sub x (currindex + 1)
          (String.length x - (currindex + 1)) in
      String.sub x 0 currindex::wordlist remainingstring
    else x::[]


  let break_line (x: string) (break: int) =
    let rec recompose (acc: string) (l: string list) (break: int) =
      match l with
      | [] -> acc
      | hd::tl ->
        if String.length (acc^hd) < break
        then recompose (acc^hd^" ") tl break
        else acc^"\n"^(recompose (hd^" ") tl break)
    in
    let ls = wordlist x in
    recompose "" ls break


end



let get_winners () =
  let score_string = ref "" in
  let rec generate_string l =
    match l with
    | [] -> !score_string
    | (bc, wc, _)::tl ->
      (FormatOps.break_line bc 50)^"\n-->"^(FormatOps.break_line wc 50)^"\n\n"^
      (generate_string tl) in
  match !winner_log with
  | None -> gamelog#set_text "No winners yet!"
  | Some s -> 
    let log = generate_string s in
    gamelog#set_text log

let get_current_score () =
  match !curr_user_state with
  | None -> "0"
  | Some s -> 
    let find_score n l = 
      List.fold_left 
        (fun score (id, s) -> if n = id then s + score else score) 0 l
    in
    string_of_int (find_score (Client.current_id ()) s.scores)


let score_dialog () =
  let icon = GdkPixbuf.from_file "res/icon.png" in
  let score = GWindow.window ~title:"Winning Cards" ~resizable:true
      ~border_width:5 ~height:600 ~width: 270 () in
  score#set_icon(Some icon);
  let myclose _ =score#destroy() in
  ignore(score#connect#destroy(myclose));
  let mainbox = GBin.scrolled_window ~packing:score#add () in
  get_winners();
  let score_num = GMisc.label ~packing:mainbox#add () in
  let winnerstext = GText.view ~buffer:gamelog
      ~justification:`FILL ~packing:mainbox#add () in
  winnerstext#set_editable(false);
  let update_gui_func () =
    upon (client_get_user_state ()) (fun curr_state -> 
        match curr_state with
        | Playing st -> 
          curr_user_state:= Some st;
          player_hand:= Some st.hand;
          winner_log:=Some st.winners;
          get_winners();
          score_num#set_label(get_current_score())
        | Judging st ->
          curr_user_state:= Some st;
          submissions:= Some st.played;
          winner_log:= Some st.winners;
          get_winners();
          score_num#set_label(get_current_score())
        | JWaiting st ->
          curr_user_state:= Some st;
          winner_log:= Some st.winners;
          get_winners();
          score_num#set_label(get_current_score())
        | PWaiting st -> 
          curr_user_state:= Some st;
          winner_log:= Some st.winners;
          get_winners();
          score_num#set_label(get_current_score())) in
  let gui_update_call = RecurringCall.create_call 1.0 update_gui_func in
  RecurringCall.start_call gui_update_call;

  let update_gui () = RecurringCall.start_call gui_update_call in
  update_gui ();
  let play_game_call = RecurringCall.create_call 1.0 start_from_json in
  RecurringCall.start_call play_game_call;
  (*Sets values of scores and items - use to integrate scoring and winning cards*)
  (*scores_list#set_text(get_scores ());*)

  score#show ()

let initial_window () = 
  ignore(locale ());
  let icon = GdkPixbuf.from_file "res/icon.png" in
  let splash = GWindow.window 
      ~resizable:false ~border_width:10 ~title:"Cards Against OCaml" () in
  splash#set_icon(Some icon);
  let main_destroy _ = splash#destroy() in
  (*ignore(splash#connect#destroy(main_destroy));*)
  let vbox = GPack.vbox ~packing:(splash#add) () in
  let logo = GdkPixbuf.from_file "res/cards.png" in
  let logo_widget = GMisc.image ~pixbuf:logo ~packing:vbox#add () in
  logo_widget#set_pixbuf logo;
  let indicator = GMisc.label ~packing:(vbox#add) () in
  indicator#set_label("Connect to server before starting game.");
  let boxbuffer = GBin.frame ~packing:(vbox#pack ~padding:5) 
      ~label:"Server address" () in
  let server = GText.buffer () in
  let server_box = GText.view ~packing:(boxbuffer#add) () in
  server#set_text("http://localhost:8080");
  server_box#set_buffer(server);
  let trainerbuffer = GBin.frame ~packing:(vbox#pack ~padding:5)
      ~label:"Trainer file" () in
  let trainer = GText.buffer () in
  trainer#set_text("trained_bot.json");
  let trainer_box = GText.view ~packing:(trainerbuffer#add) () in
  trainer_box#set_buffer(trainer);
  let connect_button = GButton.button ~label:("Click to connect bot to server!") 
      ~packing:vbox#add () in
  let start_button = GButton.button ~label:("Click to start!") 
      ~packing:vbox#add () in
  let connect_first () = 
    indicator#set_label("Connect to server first!") in

  let init_start () = 
    upon (Client.trigger_start()) (fun () -> 
        score_dialog();main_destroy()) in
  let init_connect () = 
    let server_input = server#get_text () in
    upon (connect_server server_input "string") (fun () ->
        trainer_json:=Some (Yojson.Basic.from_file (trainer#get_text()
                                       )));
        indicator#set_label("You are now connected!");
    ignore(start_button#connect#clicked ~callback:init_start);() in 
  
  ignore(connect_button#connect#clicked ~callback:(fun () -> init_connect()));
  ignore(start_button#connect#clicked ~callback:connect_first);
  (*ignore(splash#connect#destroy(confirm_exit));*)
  splash#show(); ()

let main_method () = initial_window ();
  let _ = GtkThread.start () in
  GtkThread.sync Scheduler.go ()



let _ =
  (*(match Array.to_list Sys.argv with
   | a::b::t ->
     ignore(connect_server "http://localhost:8080/" "BOT" >>=
            (fun _ -> return (start_from_json (Yojson.Basic.from_file b))))
   | _ -> failwith "insufficient argument");
    ignore(Scheduler.go ())*)
  main_method ()
