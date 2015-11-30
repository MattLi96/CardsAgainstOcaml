open Async.Std

(*int is the time, First ivar is to run the timer until it's filled,
  second Ivar is filled when timer is done*)
type timer = int ref * unit Ivar.t ref * unit Ivar.t ref

let get_time t =
  match t with
  | (i, _, _) -> !i

let bind_timer t f =
  match t with
  | (_, _, done_ivar) ->
    let _ = Ivar.read !done_ivar >>= (fun _ -> f (); return ()) in()

let pause_timer t =
  match t with
  | (_, run_ivar, _) -> Ivar.fill_if_empty !run_ivar ()

(*create timer is down here since it uses pause timer*)
let create_timer i =
  let t = (ref i, ref (Ivar.create ()), ref (Ivar.create ())) in
  pause_timer t; t

(*run the timer until the ivar is filled*)
let rec run_timer (t:timer) run_ivar =
  match t with
  | (i, r2_ivar, done_ivar) ->
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
          (fun _ -> (Log.Global.info "TIMER: %i" !i); run_timer t run_ivar; return ()) in
        ()

      )
    else ()

let start_timer t =
  match t with
  | (_, run_ivar, _) -> run_ivar := Ivar.create (); run_timer t !run_ivar

