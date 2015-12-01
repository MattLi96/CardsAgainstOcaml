open Async.Std

(*float is time in seconds to call, 
  function is the call, 
  Ivar is for stoping and starting
*)

type t = float * (unit -> unit) * unit Ivar.t ref

(*c is a call*)
let stop_call (_, _, run_ivar) =
  Ivar.fill_if_empty !run_ivar ()

let create_call t f =
  let ret = (t, f, ref (Ivar.create ())) in 
  stop_call ret; ret

let rec run_call c run_ivar =
  let (t, f, _) = c in 
  if Ivar.is_empty run_ivar then (
    f ();
    let _ = after (Core.Std.Time.Span.of_sec t) >>=
      (fun _ -> run_call c run_ivar; return ()) in 
    ()
  )
  else ()

let start_call c = 
  let (_, _, run_ivar) = c in
  Ivar.fill_if_empty !run_ivar ();
  run_ivar := Ivar.create (); 
  run_call c !run_ivar
