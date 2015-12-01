(*Module for taking care of recurring calls*)
type t

(*create call takes in *)
val create_call: float -> (unit -> unit) -> t

(*pause and start the recurring cal*)
val stop_call: t -> unit

(*start call will call immediately and then reset the call schedule*)
val start_call: t -> unit
