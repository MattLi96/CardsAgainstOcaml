open Model

type heartbeat

(*creates an initial heartbeat, cycle speed is set to int seconds*)
val create_heartbeat: int -> heartbeat

(*stops the heartbeat. Required so you can end the cycle*)
val end_heartbeat: heartbeat -> unit

(*Checks to see the users that have beated within the last 5-10 seconds.*)
val get_active_users: heartbeat -> uID list

(*The beat for a given user*)
val beat: heartbeat -> uID -> unit
