(*Module written as a dummy for running the GUI separately*)
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


  let get_deck (x: string) =
    let open Yojson.Basic.Util in
    let try_deck = try Some (Yojson.Basic.from_file x) with
        _ -> None in
    let deck = match try_deck with
      | None -> print_string "error in deck"; exit 0
      | Some d -> d in
    [deck] |> flatten |> filter_member "text" |> filter_string

  let get_random x =
    let r = Random.int ((List.length x)) in
    let rec rand_acc x n =
      if n = 0 then
        match x with
        | [] -> "error_done"
        | hd::tl -> hd
      else
        match x with
        | [] -> "error_endearly"
        | hd::tl -> rand_acc tl (n-1)
    in rand_acc x r

  let get_random_break x n =
    break_line (get_random x) n


end

(*Open system files*)
open Client
open State
open RecurringCall
open Async.Std

let flush () = after(Core.Std.sec 0.)

(*GTK Initializations*)
let locale () = GtkMain.Main.init ()
let destroy () = GMain.Main.quit ()

(*Global references to decks, hands, etc*)
let score_visible = ref None
let about_visible = ref None
let expansion_enabled = ref false
let judging_mode = ref false
(*let init_state = ref (client_get_user_state ())*)
let (curr_user_state: State.univ_c_state option ref) = ref None
let player_hand = ref None
let submissions = ref None
let gamelog = GText.buffer ()
let winner_log = ref None


let format_scores () =
  let rec format_list l =
    match l with
    | (u,s)::tl -> (string_of_int u)^": "
                   ^(string_of_int s)^"\n"^(format_list tl)
    | [] -> "" in
  match !curr_user_state with
  | None -> "Error: invalid data"
  | Some s -> format_list (s.scores)

let rec find_idx n l =
  match l with
  | [] -> ""
  | hd::tl -> if n = 1 then hd else find_idx (n-1) tl

let get_current_score () =
  match !curr_user_state with
  | None -> "0"
  | Some s -> 
    let find_score n l = 
      List.fold_left 
        (fun score (id, s) -> if n = id then s + score else score) 0 l
    in
    string_of_int (find_score (Client.current_id ()) s.scores)


let rec get_submissions x =
  match !submissions with
  | None -> "Waiting on submissions"
  | Some ls -> 
    find_idx x (List.map snd ls)

let rec get_hand_num x l =
  match l with
  | None -> "Waiting on server"
  | Some hand ->
    FormatOps.break_line (find_idx x (hand)) 22

let rec get_submit_num x l =
  match l with
  | None -> "Waiting on server"
  | Some hand -> (find_idx x (hand))

let submit_hand_num n = 
  get_submit_num n !player_hand

let submit_judge_num n =
  get_submissions n

let get_curr_bl () =
  match !curr_user_state with
  | None -> "Waiting on server"
  | Some x -> x.b_card

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


(*Options window, currently only contains one option*)
(*let options_window () =
  let option_window = GWindow.window ~title:"Options" ~resizable:false
      ~border_width:5 () in
  let myclose _ = option_window#destroy() in
  ignore(option_window#connect#destroy ~callback:(myclose));
  let vbox = GPack.vbox ~spacing:5 ~packing:option_window#add () in
  let info = GMisc.label ~line_wrap:true
    (*~text:"Enable/disable expansion pack.  Warning: This will reset your
      current game and all data will be lost."*) ~packing:vbox#add () in
  let checkbox = GButton.check_button ~active:(!expansion_enabled)
      ~packing:vbox#add () in
  let confirm = GButton.button ~packing:vbox#add ~label:"Confirm!"() in
  info#set_text("Enable/disable expansion packs. Warning: This will reset your
                 current game and all data will be lost.");
  checkbox#set_label("Use expansion packs!");
  let update_expansion () = expansion_enabled:= (checkbox#active) in
  ignore(confirm#connect#clicked(update_expansion));
  ignore(confirm#connect#clicked(myclose));
  option_window#show ()*)


(*About screen - includes image, about text*)
let about_screen () =
  match !about_visible with
  |None ->
    let icon = GdkPixbuf.from_file "res/icon.png" in
    let about = GWindow.window ~title:"About" ~resizable:false
        ~border_width:5 () in
    about#set_icon (Some icon);
    let myclose _ = about_visible:=None;about#destroy () in
    ignore(about#connect#destroy(myclose));
    let vbox = GPack.vbox  ~packing:about#add() in
    let logo = GdkPixbuf.from_file "res/cards.png" in
    let logo_widget = GMisc.image ~pixbuf:logo ~packing:vbox#add () in
    logo_widget#set_pixbuf logo;
    let aboutlabel = GMisc.label ~line_wrap:true ~packing:vbox#add
        ~justify:`CENTER() in
    aboutlabel#set_text("0.9.5r12215\n\nCharley Chen\nMatthew Li\nAustin Liu
Jared Wong\n
Some code borrowed from the open-source lablgtk2 libraries.\n
 2015. All rights reserved.");
    let okbutton = GButton.button ~packing:vbox#add () in
    okbutton#set_label("Close");
    ignore(okbutton#connect#clicked(myclose));
    about_visible:= Some about;
    about#show ()
  |Some s -> ()

(*Score screen popup - includes current scores, and a list of winning cards,
 *black cards, and the players who won the cards*)

let score_dialog () =
  let icon = GdkPixbuf.from_file "res/icon.png" in
  let score = GWindow.window ~title:"Winning Cards" ~resizable:true
      ~border_width:5 ~height:600 ~width: 270 () in
  score#set_icon(Some icon);
  score_visible:= Some score;
  let myclose _ =score_visible:=None;score#destroy() in
  ignore(score#connect#destroy(myclose));
  let mainbox = GBin.scrolled_window ~packing:score#add () in
  get_winners();
  let winnerstext = GText.view ~buffer:gamelog
      ~justification:`FILL ~packing:mainbox#add () in
  winnerstext#set_editable(false);

  (*Sets values of scores and items - use to integrate scoring and winning cards*)
  (*scores_list#set_text(get_scores ());*)

  score#show ()



let main_window () =
  ignore(locale ());
  let icon = GdkPixbuf.from_file "res/icon.png" in
  let window = GWindow.window
      ~resizable:false ~border_width:0 ~title:"Cards Against OCaml"() in
  window#set_icon(Some icon);
  let main_destroy _ = destroy();ignore(exit 0);() in
  ignore(window#connect#destroy(main_destroy));
  let menubox = GPack.vbox ~packing:window#add () in
  let menubar = GMenu.menu_bar ~packing:menubox#pack() in
  let hbox = GPack.hbox ~packing:(menubox#pack ~padding:50) () in
  let windowbox = GPack.vbox ~packing:(hbox#pack ~padding:50) () in
  let hbox_top = GPack.hbox ~packing:(windowbox#pack ~padding:0) () in
  let current_mode = GMisc.label ~packing:(hbox_top#pack ~padding:5) () in
  let logo = GdkPixbuf.from_file "res/logo.png" in
  let logo_widget = GMisc.image ~pixbuf:logo ~packing:hbox_top#add () in
  logo_widget#set_pixbuf logo;
  (*let opt_menu = GMenu.menu () in
  let opt_button = GMenu.menu_item ~label:("More settings")
      ~packing:opt_menu#append () in
  ignore(opt_button#connect#activate(options_window));
  let menu_opts = GMenu.menu_item ~label:"Options" 
      ~packing:menubar#append () in
  menu_opts#set_submenu (opt_menu);*)
  let about_menu = GMenu.menu () in
  let about_button = GMenu.menu_item ~label:("About")
      ~packing:about_menu#append () in
  ignore(about_button#connect#activate ~callback:about_screen);
  let exit_button = GMenu.menu_item ~label:("Exit")
      ~packing:about_menu #append () in
  ignore(exit_button#connect#activate ~callback:destroy);
  let menu_about = GMenu.menu_item ~label:"About" ~packing:menubar#append() in
  menu_about#set_submenu(about_menu);

  let bcbox = GPack.hbox ~packing:(windowbox#pack ~padding:10) () in
  let mhbox = GPack.vbox ~packing:(windowbox#pack ~padding:0) () in
  let timerframe = GBin.frame ~packing:(bcbox#pack ~padding:5)
      ~label: "Timer:" ~width: 70 ~height:70 () in
  let timer = GMisc.label ~packing:(timerframe#add) () in
  let bcframe = GBin.frame ~packing:(bcbox#pack ~padding:50)
      ~label:"Current Black Card" ~width:540 ~height:95 () in
  let scoreframe = GBin.frame ~packing:(bcbox#pack ~padding:5)
      ~label: "Score:" ~width:70 ~height:70 () in
  let show_score = GButton.button ~packing:(hbox_top#pack ~padding:5) 
      ~label:("Show scores") () in
  let score_dialog_opt () =
    match !score_visible with
    |None -> score_dialog ()
    |Some window -> window#destroy () in
  ignore(show_score#connect#clicked(score_dialog_opt));
  let score = GMisc.label ~packing:(scoreframe#add) () in
  let cbox1 = GPack.hbox ~packing:(mhbox#pack ~padding:0) () in
  let cbox2 = GPack.hbox ~packing:(mhbox#pack ~padding:0) () in
  let card1box = GPack.vbox ~packing:(cbox1#pack ~padding:0) () in
  let card2box = GPack.vbox ~packing:(cbox1#pack ~padding:0) () in
  let card3box = GPack.vbox ~packing:(cbox1#pack ~padding:0) () in
  let card4box = GPack.vbox ~packing:(cbox1#pack ~padding:0) () in
  let card5box = GPack.vbox ~packing:(cbox1#pack ~padding:0) () in
  let card6box = GPack.vbox ~packing:(cbox2#pack ~padding:0) () in
  let card7box = GPack.vbox ~packing:(cbox2#pack ~padding:0) () in
  let card8box = GPack.vbox ~packing:(cbox2#pack ~padding:0) () in
  let card9box = GPack.vbox ~packing:(cbox2#pack ~padding:0) () in
  let card10box = GPack.vbox ~packing:(cbox2#pack ~padding:0) () in
  let card1frame = GBin.frame ~packing:(card1box#pack ~padding:0)
      ~width:160 ~height:160 () in
  let card2frame = GBin.frame ~packing:(card2box#pack ~padding:0)
      ~width:160 ~height:160 () in
  let card3frame = GBin.frame ~packing:(card3box#pack ~padding:0)
      ~width:160 ~height:160 () in
  let card4frame = GBin.frame ~packing:(card4box#pack ~padding:0)
      ~width:160 ~height:160 () in
  let card5frame = GBin.frame ~packing:(card5box#pack ~padding:0)
      ~width:160 ~height:160 () in
  let card6frame = GBin.frame ~packing:(card6box#pack ~padding:0)
      ~width:160 ~height:160 () in
  let card7frame = GBin.frame ~packing:(card7box#pack ~padding:0)
      ~width:160 ~height:160 () in
  let card8frame = GBin.frame ~packing:(card8box#pack ~padding:0)
      ~width:160 ~height:160 () in
  let card9frame = GBin.frame ~packing:(card9box#pack ~padding:0)
      ~width:160 ~height:160 () in
  let card10frame = GBin.frame ~packing:(card10box#pack ~padding:0)
      ~width:160 ~height:160 () in

  let btext = "Waiting on server" in
  let bcard = GMisc.label ~packing:bcframe#add ~line_wrap:true () in
  let card1 = GButton.button ~label:btext
      ~packing:(card1frame#add) ~relief:`NONE () in
  card1#set_focus_on_click(false);
  let card2 = GButton.button ~label:btext
      ~packing:(card2frame#add) ~relief:`NONE () in
  card2#set_focus_on_click(false);
  let card3 = GButton.button ~label:btext
      ~packing:(card3frame#add) ~relief:`NONE () in
  card3#set_focus_on_click(false);
  let card4 = GButton.button ~label:btext
      ~packing:(card4frame#add) ~relief:`NONE () in
  card4#set_focus_on_click(false);
  let card5 = GButton.button ~label:btext
      ~packing:(card5frame#add) ~relief:`NONE () in
  card5#set_focus_on_click(false);
  let card6 = GButton.button ~label:btext
      ~packing:(card6frame#add) ~relief:`NONE () in
  card6#set_focus_on_click(false);
  let card7 = GButton.button ~label:btext
      ~packing:(card7frame#add) ~relief:`NONE () in
  card7#set_focus_on_click(false);
  let card8 = GButton.button ~label:btext
      ~packing:(card8frame#add) ~relief:`NONE () in
  card8#set_focus_on_click(false);
  let card9 = GButton.button ~label:btext
      ~packing:(card9frame#add) ~relief:`NONE () in
  card9#set_focus_on_click(false);
  let card10 = GButton.button ~label:btext
      ~packing:(card10frame#add) ~relief:`NONE() in
  card10#set_focus_on_click(false);


  (*Code for initialization, can be modified to take data from the server
   *when this is fully implemented.*)

  (*Universal Callbacks: Methods for updating score, updating timer -
   *bound to every button currently, primarily for debug.  Can be modified*)


  let update_gui_func () =
    upon (client_get_user_state ()) (fun curr_state ->
        match curr_state with
        | Playing st->
          judging_mode:=false;
          curr_user_state:= Some st;
          player_hand:= Some st.hand;
          winner_log:=Some st.winners;
          get_winners();
          let h = Some st.hand in
          card1#set_label(get_hand_num 1 h);
          card2#set_label(get_hand_num 2 h);
          card3#set_label(get_hand_num 3 h);
          card4#set_label(get_hand_num 4 h);
          card5#set_label(get_hand_num 5 h);
          card6#set_label(get_hand_num 6 h);
          card7#set_label(get_hand_num 7 h);
          card8#set_label(get_hand_num 8 h);
          card9#set_label(get_hand_num 9 h);
          card10#set_label(get_hand_num 10 h);
          current_mode#set_label("Pick the best card!");
          bcard#set_label(st.b_card);
          score#set_label(get_current_score())
        | Judging st ->
          judging_mode:=true;
          curr_user_state:= Some st;
          submissions:= Some st.played;
          winner_log:=Some st.winners;
          get_winners(); 
          card1#set_label(FormatOps.break_line (get_submissions 1) 22);
          card2#set_label(FormatOps.break_line (get_submissions 2) 22);
          card3#set_label(FormatOps.break_line (get_submissions 3) 22);
          card4#set_label(FormatOps.break_line (get_submissions 4) 22);
          card5#set_label(FormatOps.break_line (get_submissions 5) 22);
          card6#set_label(FormatOps.break_line (get_submissions 6) 22);
          card7#set_label(FormatOps.break_line (get_submissions 7) 22);
          card8#set_label(FormatOps.break_line (get_submissions 8) 22);
          card9#set_label(FormatOps.break_line (get_submissions 9) 22);
          card10#set_label(FormatOps.break_line (get_submissions 10) 22);
          bcard#set_label (st.b_card);
          score#set_label(get_current_score());
          current_mode#set_label("You are the Czar!") 
        | JWaiting st ->
          judging_mode:=true;
          curr_user_state:= Some st;
          winner_log:= Some st.winners;
          get_winners();
          card1#set_label("Waiting for players");
          card2#set_label("Waiting for players");
          card3#set_label("Waiting for players");
          card4#set_label("Waiting for players");
          card5#set_label("Waiting for players");
          card6#set_label("Waiting for players");
          card7#set_label("Waiting for players");
          card8#set_label("Waiting for players");
          card9#set_label("Waiting for players");
          card10#set_label("Waiting for players");
          bcard#set_label("Waiting for players");
          current_mode#set_label("You are the Czar!");
          score#set_label(get_current_score())
        | PWaiting st ->
          judging_mode:=false;
          curr_user_state:= Some st;
          winner_log:= Some st.winners; 
          card1#set_label("Waiting for czar");
          card2#set_label("Waiting for czar");
          card3#set_label("Waiting for czar");
          card4#set_label("Waiting for czar");
          card5#set_label("Waiting for czar");
          card6#set_label("Waiting for czar");
          card7#set_label("Waiting for czar");
          card8#set_label("Waiting for czar");
          card9#set_label("Waiting for czar");
          card10#set_label("Waiting for czar");
          bcard#set_label("Waiting for czar");
          current_mode#set_label("Pick the best card!"));
          score#set_label(get_current_score())
  in
  let gui_update_call = RecurringCall.create_call 1.0 update_gui_func in
  RecurringCall.start_call gui_update_call;
  let timer_update_call = RecurringCall.create_call 0.2  
      (fun () -> timer#set_label(string_of_int (Client.get_time ()))) 
  in
  RecurringCall.start_call timer_update_call;

  let update_gui () = RecurringCall.start_call gui_update_call in

  (*Callbacks: Here is where the callbacks are assigned for each
   *of the 10 buttons in the main interface of the GUI.  When connecting
   *to the server, each of the 10 buttons' callbacks can be used to call
   *functions which deal with card data from the server.  In the dummy,
   *all callbacks are identical.  Later, each button can be assigned to
   *additional callbacks to transmit which card was selected to the server.*)

  let callback1 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 1)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 1)) (fun () ->
          update_gui()) in
  let callback2 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 2)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 2)) (fun () ->
          update_gui()) in
  let callback3 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 3)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 3)) (fun () ->
          update_gui()) in
  let callback4 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 4)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 4)) (fun () ->
          update_gui()) in
  let callback5 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 5)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 5)) (fun () ->
          update_gui())in
  let callback6 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 6)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 6)) (fun () ->
          update_gui()) in
  let callback7 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 7)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 7)) (fun () ->
          update_gui()) in
  let callback8 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 8)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 8)) (fun () ->
          update_gui()) in
  let callback9 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 9)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 9)) (fun () ->
          update_gui()) in
  let callback10 () = bcard#set_label("Waiting for other players");
    if !judging_mode = true 
    then (upon (client_judge (submit_judge_num 10)) (fun () -> update_gui ())) 
    else upon (client_play_white (submit_hand_num 10)) (fun () ->
          update_gui()) in

  ignore(window#connect#destroy ~callback:main_destroy);
  ignore(card1#connect#clicked ~callback:callback1);
  ignore(card2#connect#clicked ~callback:callback2);
  ignore(card3#connect#clicked ~callback:callback3);
  ignore(card4#connect#clicked ~callback:callback4);
  ignore(card5#connect#clicked ~callback:callback5);
  ignore(card6#connect#clicked ~callback:callback6);
  ignore(card7#connect#clicked ~callback:callback7);
  ignore(card8#connect#clicked ~callback:callback8);
  ignore(card9#connect#clicked ~callback:callback9);
  ignore(card10#connect#clicked ~callback:callback10);
  window#show()

(*let confirm_exit () = 
  let icon = GdkPixbuf.from_file "res/icon.png" in
  let conf = GWindow.window 
      ~resizable:false ~border_width:0 ~title:"Leaving?" () in
  conf#set_icon(Some icon);
  let vbox = GPack.vbox ~packing:(conf#add) () in
  let txt = GMisc.label ~packing:(vbox#add) () in
  let hbox = GPack.hbox ~packing:(vbox#add) () in
  let yes = GButton.button ~packing:(hbox#add) ~label:("Yes")() in
  let no = GButton.button ~packing:(hbox#add) ~label:("No")() in
  txt#set_label("Are you sure you want to leave?");
  ignore(no#connect#clicked(fun () -> conf#destroy()));
  ignore(yes#connect#clicked(fun () -> (destroy();ignore(exit 0);())));
  conf#show()*)



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
  let connect_button = GButton.button ~label:("Click to connect to server!") 
      ~packing:vbox#add () in
  let start_button = GButton.button ~label:("Click to start!") 
      ~packing:vbox#add () in
  let init_start () = 
    upon (Client.trigger_start()) (fun () -> (main_window();main_destroy())) in
  let init_connect () = 
    let server_input = server#get_text () in
    let connect_attempt = try Some (connect_server server_input "string") with
      | _ -> None in
    match connect_attempt with
    | Some d ->
      upon d (fun () ->
          ignore(start_button#connect#clicked ~callback:init_start);
          indicator#set_label("You are now connected!")) 
    | None -> indicator#set_label("Error, could not connect. Try again!") in

  let connect_first () = 
    indicator#set_label("Connect to server first!") in

  ignore(connect_button#connect#clicked ~callback:init_connect);
  ignore(start_button#connect#clicked ~callback:connect_first);
  (*ignore(splash#connect#destroy(confirm_exit));*)
  splash#show(); ()

let main () =
  initial_window ();
  let _ = GtkThread.start () in
  GtkThread.sync Scheduler.go ()

let _ = main ()
