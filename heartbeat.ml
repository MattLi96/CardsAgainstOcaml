open Model (*get type uID*)
open Async.Std

(*
The int is the time in seconds of a cycle
The Ivar is for ending the cyling. When you fill the Ivar the cyling stops   
The first list is the list of "active users"
The second list is the list of users who have called beat since the last cycle

Every i seconds, the first list is dumped and
   the second list becomes the first list. This
   is how we keep track of the active users.
*)
type heartbeat = int * unit Ivar.t * uID list ref * uID list ref

(*cycle is the end of 1 heartbeat cycle. It dumps list 1 and 
  clears list 2*)
let rec cycle hb =
  match hb with
  | (i, ivar, l1, l2) -> 
    if Ivar.is_full ivar then () 
    else (

      l1 := !l2; l2 := [];
      let _ = after (Core.Std.Time.Span.of_int_sec i) >>= 
        (fun _ -> cycle hb; return ()) in
      ()
    )


let create_heartbeat i = 
  let hb = (i, Ivar.create (),ref [], ref []) in
  cycle hb; hb

let end_heartbeat hb =
  match hb with
  | (_, ivar, _, _) -> Ivar.fill_if_empty ivar ()

let get_active_users hb = 
  match hb with
  | (_, _, l, _) -> !l

let beat hb uid =
  match hb with
  | (_, _, _, l) -> l := (uid::!l)
