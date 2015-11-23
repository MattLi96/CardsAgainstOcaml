module PullCards = struct 

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



let locale() = GtkMain.Main.init()

let destroy () = GMain.Main.quit()
let button_callback () = (print_endline "BUTTONCALLBACK")

let button2c() = button_callback(); flush stdout
let button3c() = (print_endline "Button 3 pressed"); flush stdout
let button4c() = (print_endline "Button 4 pressed"); flush stdout
let button5c() = (print_endline "Button 5 pressed"); flush stdout


let expansion_enabled = ref false

let new_black_card() = 
  let blackdeck = PullCards.get_deck "black.json" in
  PullCards.get_random_break blackdeck 50

let new_white_card() =
  let whitedeck = PullCards.get_deck "white.json" in
  PullCards.get_random_break whitedeck 20

(*Demo code from testgtk.ml*)
(*let create_menu depth tearoff =
  let rec aux depth tearoff =
    let menu = GMenu.menu () and group = ref None in
    if tearoff then ignore (GMenu.tearoff_item ~packing: menu#append ());
    for i = 0 to 4 do
      let menuitem = GMenu.radio_menu_item ?group:!group
          ~label:("item " ^ string_of_int depth ^ " - " ^ string_of_int (i+1))
          ~packing:menu#append ~show_toggle:(depth mod 2 <> 0)
          () in
      group := Some (menuitem #group);
      if i = 3 then menuitem #misc#set_sensitive false;
      if depth > 1 then
        menuitem #set_submenu (aux (depth-1) true)
    done;

    menu
  in aux depth tearoff*)

let options_window () =
  let option_window = GWindow.window ~title:"Options" ~border_width:5 () in
  let myclose _ = option_window#destroy() in
  ignore(option_window#connect#destroy ~callback:(myclose));
  let vbox = GPack.vbox ~spacing:5 ~packing:option_window#add () in
  let info = GMisc.label ~line_wrap:true 
      (*~text:"Enable/disable expansion pack.  Warning: This will reset your
      current game and all data will be lost."*) ~packing:vbox#add () in 
  let checkbox = GButton.check_button ~active:(!expansion_enabled) ~packing:vbox#add () in
  let confirm = GButton.button ~packing:vbox#add ~label:"Confirm!"() in
  info#set_text("Enable/disable expansion packs. Warning: This will reset your
                 current game and all data will be lost.");
  checkbox#set_label("Use expansion packs!");
  let update_expansion() = expansion_enabled:= (checkbox#active) in
  ignore(confirm#connect#clicked(update_expansion));
  ignore(confirm#connect#clicked(myclose));
  option_window#show()

let about_screen () =
  let about = GWindow.window ~title:"About" ~border_width:5 () in
  let myclose _ = about#destroy() in
  ignore(about#connect#destroy(myclose));
  let vbox = GPack.vbox ~spacing:5 ~packing:about#add() in
  let logo = GdkPixbuf.from_file "cards.jpg" in
  let logo_widget = GMisc.image ~pixbuf:logo ~packing:vbox#add () in
  logo_widget#set_pixbuf logo;
  let aboutlabel = GMisc.label ~line_wrap:true ~packing:vbox#add ~justify:`CENTER() in
  aboutlabel#set_text("Cards Against Ocaml\nv0.0.05a112215\n\nCharley Chen\nMatthew Li\nAustin Liu \nJared Wong\n
Some code borrowed from the open-source lablgtk2 libraries.\n\n 2015. All rights reserved.");

about#show()



let main () =
  ignore(locale());
  let curr_val = ref 30 in
  let icon = GdkPixbuf.from_file "icon.png" in
  let window = GWindow.window 
      ~border_width:0 ~title:"Cards Against OCaml"() in
  window#set_icon(Some icon);
  let windowbox = GPack.vbox ~packing:window#add() in
  let menubar = GMenu.menu_bar ~packing:windowbox#pack() in
  let logo = GdkPixbuf.from_file "logo.jpg" in
  let logo_widget = GMisc.image ~pixbuf:logo ~packing:windowbox#add () in
  logo_widget#set_pixbuf logo; 
  let opt_menu = GMenu.menu() in
  let opt_button = GMenu.menu_item ~label:("More settings") ~packing:opt_menu#append() in
  ignore(opt_button#connect#activate(options_window));
  let menu_opts = GMenu.menu_item ~label:"Options" ~packing:menubar#append() in
  menu_opts#set_submenu (opt_menu);
  let about_menu = GMenu.menu() in
  let about_button = GMenu.menu_item ~label:("About") ~packing:about_menu#append() in
  ignore(about_button#connect#activate ~callback:about_screen);
  let exit_button = GMenu.menu_item ~label:("Exit") ~packing:about_menu #append() in
  ignore(exit_button#connect#activate ~callback:destroy);
  let menu_about = GMenu.menu_item ~label:"About" ~packing:menubar#append() in
  menu_about#set_submenu(about_menu);

  (*let title = GMisc.label ~packing:(windowbox#pack ~padding:10)() in*)
  let bcbox = GPack.hbox ~packing:(windowbox#pack ~padding:10)() in
  let mhbox = GPack.vbox ~packing:(windowbox#pack ~padding:5)() in
  let timerframe = GBin.frame ~packing:(bcbox#pack ~padding:5) ~label: "Timer:" ~width: 70 ~height:70 () in
  let timer = GMisc.label ~packing:(timerframe#add)() in
  let bcframe = GBin.frame ~packing:(bcbox#pack ~padding:50) ~label:"Current Black Card" ~width:540 ~height:70 ()in
  let scoreframe = GBin.frame ~packing:(bcbox#pack ~padding:5) ~label: "Score:" ~width:70 ~height:70() in
  let score = GMisc.label ~packing:(scoreframe#add)() in
  let cbox1 = GPack.hbox ~packing:(mhbox#pack ~padding:5)() in
  let cbox2 = GPack.hbox ~packing:(mhbox#pack ~padding:5)() in
  let card1box = GPack.vbox ~packing:(cbox1#pack ~padding:0)() in
  let card2box = GPack.vbox ~packing:(cbox1#pack ~padding:0)() in
  let card3box = GPack.vbox ~packing:(cbox1#pack ~padding:0)() in
  let card4box = GPack.vbox ~packing:(cbox1#pack ~padding:0)() in
  let card5box = GPack.vbox ~packing:(cbox1#pack ~padding:0)() in
  let card6box = GPack.vbox ~packing:(cbox2#pack ~padding:0)() in
  let card7box = GPack.vbox ~packing:(cbox2#pack ~padding:0)() in
  let card8box = GPack.vbox ~packing:(cbox2#pack ~padding:0)() in
  let card9box = GPack.vbox ~packing:(cbox2#pack ~padding:0)() in
  let card10box = GPack.vbox ~packing:(cbox2#pack ~padding:0)() in
  let card1frame = GBin.frame ~packing:(card1box#pack ~padding:15) ~width:160 ~height:160() in
  let card2frame = GBin.frame ~packing:(card2box#pack ~padding:15) ~width:160 ~height:160() in
  let card3frame = GBin.frame ~packing:(card3box#pack ~padding:15) ~width:160 ~height:160() in
  let card4frame = GBin.frame ~packing:(card4box#pack ~padding:15) ~width:160 ~height:160() in
  let card5frame = GBin.frame ~packing:(card5box#pack ~padding:15) ~width:160 ~height:160() in
  let card6frame = GBin.frame ~packing:(card6box#pack ~padding:15) ~width:160 ~height:160() in
  let card7frame = GBin.frame ~packing:(card7box#pack ~padding:15) ~width:160 ~height:160() in
  let card8frame = GBin.frame ~packing:(card8box#pack ~padding:15) ~width:160 ~height:160() in
  let card9frame = GBin.frame ~packing:(card9box#pack ~padding:15) ~width:160 ~height:160() in
  let card10frame = GBin.frame ~packing:(card10box#pack ~padding:15) ~width:160 ~height:160() in

  let btext = "Select!" in
  let bcard = GMisc.label ~packing:bcframe#add ~line_wrap:true() in
  let card1 = GMisc.label  ~packing:card1frame#add ~line_wrap:true() in
  let button1 = GButton.button ~label:btext ~packing:(card1box#pack ~padding:0)() in
  let card2 = GMisc.label ~packing:card2frame#add ~line_wrap:true () in
  let button2 = GButton.button ~label:btext ~packing:(card2box#pack ~padding:0)() in
  let card3 = GMisc.label ~packing:card3frame#add ~line_wrap:true () in
  let button3 = GButton.button ~label:btext ~packing:(card3box#pack ~padding:0)() in
  let card4 = GMisc.label ~packing:card4frame#add ~line_wrap:true () in
  let button4 = GButton.button ~label:btext ~packing:(card4box#pack ~padding:0)() in
  let card5 = GMisc.label ~packing:card5frame#add ~line_wrap:true () in
  let button5 = GButton.button ~label:btext ~packing:(card5box#pack ~padding:0)() in
  let card6 = GMisc.label ~packing:card6frame#add ~line_wrap:true () in
  let button6 = GButton.button ~label:btext ~packing:(card6box#pack ~padding:0)() in
  let card7 = GMisc.label ~packing:card7frame#add ~line_wrap:true () in
  let button7 = GButton.button ~label:btext ~packing:(card7box#pack ~padding:0)() in
  let card8 = GMisc.label ~packing:card8frame#add ~line_wrap:true () in
  let button8 = GButton.button ~label:btext ~packing:(card8box#pack ~padding:0)() in
  let card9 = GMisc.label ~packing:card9frame#add ~line_wrap:true () in
  let button9 = GButton.button ~label:btext ~packing:(card9box#pack ~padding:0)() in
  let card10 = GMisc.label ~packing:card10frame#add ~line_wrap:true () in
  let button10 = GButton.button ~label:btext ~packing:(card10box#pack ~padding:0)() in


  (*title#set_label("Cards Against OCaml");*)
  (*logo#set_image("cah.jpg");*)
  (*bcard#set_label("Black Card");
    card1#set_label("Card 1 is a very very very long card");
    card2#set_label("Card 2");
    card3#set_label("Card 3");
    card4#set_label("Card 4");
    card5#set_label("Card 5");*)
  timer#set_label((string_of_int(!curr_val)));
  score#set_label("score");
  let set_new_cards() = 
    card1#set_label(new_white_card());
    card2#set_label(new_white_card());
    card3#set_label(new_white_card());
    card4#set_label(new_white_card());
    card5#set_label(new_white_card());
    bcard#set_label(new_black_card()) in
  card6#set_label(new_white_card());
  card7#set_label(new_white_card());
  card8#set_label(new_white_card());
  card9#set_label(new_white_card());
  card10#set_label(new_white_card());

  set_new_cards();
  let cb1 ()= card1#set_label(new_white_card());
    bcard#set_label(new_black_card()); in
  let cb2 ()= card2#set_label(new_white_card());
    bcard#set_label(new_black_card()); in
  let cb3 ()= card3#set_label(new_white_card());
    bcard#set_label(new_black_card()); in
  let cb4 ()= card4#set_label(new_white_card());
    bcard#set_label(new_black_card()); in
  let cb5 ()= card5#set_label(new_white_card());
    bcard#set_label(new_black_card()); in
  let cb6 ()= card6#set_label(new_white_card());
    bcard#set_label(new_black_card()); in
  let cb7 ()= card7#set_label(new_white_card());
    bcard#set_label(new_black_card()); in
  let cb8 ()= card8#set_label(new_white_card());
    bcard#set_label(new_black_card()); in
  let cb9 ()= card9#set_label(new_white_card());
    bcard#set_label(new_black_card()); in
  let cb10 ()= card10#set_label(new_white_card());
    bcard#set_label(new_black_card()); in

  (*let time = ref 30 in
    let rec timertest () = Async.upon (Async.after Core.Std.sec 1.0) (fun () -> time:= (!time-1);
                                                        timer#set_label(string_of_int(!time))); timertest() in timertest;*)

  ignore(window#connect#destroy ~callback:(destroy));
  ignore(button1#connect#clicked ~callback:(fun () -> cb1()));
  ignore(button2#connect#clicked ~callback:(fun () -> cb2()));
  ignore(button3#connect#clicked ~callback:(fun () -> cb3()));
  ignore(button4#connect#clicked ~callback:(fun () -> cb4()));
  ignore(button5#connect#clicked ~callback:(fun () -> cb5()));
  ignore(button6#connect#clicked ~callback:(fun () -> cb6()));
  ignore(button7#connect#clicked ~callback:(fun () -> cb7()));
  ignore(button8#connect#clicked ~callback:(fun () -> cb8()));
  ignore(button9#connect#clicked ~callback:(fun () -> cb9()));
  ignore(button10#connect#clicked ~callback:(fun () -> cb10()));
  window#show();
  GMain.Main.main()

let _ = main()
