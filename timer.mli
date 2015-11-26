(*timer*)
type timer

(*create_timer takes in an integer and gives back a timer with the given
number of seconds*)
val create_timer: int -> timer

(*get_timer takes in a timer object and returns the number of seconds
remaining*)
val get_timer: timer -> int

(*pause and play the timer*)
val pause_timer: timer -> unit

val start_timer: timer -> unit
