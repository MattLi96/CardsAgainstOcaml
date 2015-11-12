(*Server*)
type uID

(*Authentication: All functions related to user authentication*)
(*login takes in a username and returns a JSON containing the 
 *corresponding uID and other related information*)
val login: string -> json

(*logout - takes a uID and logs it out of the game*)
val logout: uID -> unit

(*reset - sends a request to the server that destroys all of
 *the user's data*)
val reset: uID -> unit

(*heartbeat - takes a username and*)
val heartbeat: uID -> unit
val get_active: unit -> uID list
