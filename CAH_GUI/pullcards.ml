module PullCards = struct 

  type cardsfile = Yojson.Basic.json

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


end 
