open Model (*get type uID*)
open Async.Std
open RecurringCall

(*
The Ivar is for ending the cyling. When you fill the Ivar the cyling stops   
The first list is the list of "active users"
The second list is the list of users who have called beat since the last cycle

Every i seconds, the first list is dumped and
   the second list becomes the first list. This
   is how we keep track of the active users.
*)
type heartbeat = unit Ivar.t * uID list ref * uID list ref

(*cycle is the end of 1 heartbeat cycle. It dumps list 1 and 
  clears list 2*)
let cycle hb =
  let (_, l1, l2) = hb in
  l1 := !l2; l2 := []


let create_heartbeat i = 
  let ivar = Ivar.create () in
  let hb = (ivar, ref [], ref []) in
  let call = create_call (float_of_int i) (fun () -> cycle hb) in
  start_call call;
  upon (Ivar.read ivar) (fun () -> stop_call call);
  hb

let end_heartbeat hb =
  match hb with
  | (ivar, _, _) -> Ivar.fill_if_empty ivar ()

(*Helper function for removing duplicates*)
let rec remove_dup lst = 
  match lst with 
  | [] -> []
  | h::t -> h::(remove_dup (List.filter (fun x -> x<>h) t))

let get_active_users hb = 
  match hb with
  | (_, l, _) -> !l (*l := remove_dup !l; !l*)

let beat hb uid =
  match hb with
  | (_, l1, l2) -> l1 := (uid::!l1);l2 := (uid::!l2)


