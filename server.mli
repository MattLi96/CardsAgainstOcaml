(*Server*)
type uID

(*Authentication: All functions related to user authentication*)
(*login takes in a username and *)
val login: string -> json

(*logout *)
val logout: uID -> unit

(*reset*)
val reset: uID -> unit