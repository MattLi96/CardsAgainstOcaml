open Async.Std

(*int is the time, First ivar is to run the timer until it's filled,
  second Ivar is filled when timer is done*)
type timer = int ref * unit Ivar.t ref * unit Ivar.t ref

let get_time (i, _, _) =
  !i

let bind_timer (_, _, done_ivar) f =
  let _ = Ivar.read !done_ivar >>= (fun _ -> f (); return ()) in
  ()

let pause_timer (_, run_ivar, _) =
  Ivar.fill_if_empty !run_ivar ()

(*create timer is down here since it uses pause timer*)
let create_timer i =
  let t = (ref i, ref (Ivar.create ()), ref (Ivar.create ())) in
  pause_timer t; t

(*run the timer until the ivar is filled*)
let rec run_timer (t:timer) run_ivar =
  let (i, r2_ivar, done_ivar) = t in
  if Ivar.is_empty run_ivar then
    if !i <= 0 then (
      (Ivar.fill_if_empty run_ivar ());
      (Ivar.fill_if_empty !r2_ivar ()); (*this fill is just in case*)
      (Ivar.fill_if_empty !done_ivar ());
      ()
    )
    else (
      i := (!i-1);
      let _ = after (Core.Std.Time.Span.of_sec 1.0) >>=
        (fun _ -> run_timer t run_ivar; return ()) in
      ()

    )
  else ()

let start_timer t =
  let (_, run_ivar, _) = t in
  Ivar.fill_if_empty !run_ivar ();
  run_ivar := Ivar.create (); run_timer t !run_ivar

