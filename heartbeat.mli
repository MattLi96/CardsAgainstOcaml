open Model

type heartbeat

val get_active_users: hearbeat -> uID list

val beat: heartbeat -> uID -> ()
