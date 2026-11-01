open Core
open Async

type write_fn = Message.t -> unit Deferred.t

val with_connection
  :  uri:Uri.t
  -> game_lobby:string
  -> token:string
  -> on_connect:(write:write_fn -> unit Deferred.t)
  -> f:(Message.t -> write:write_fn -> unit Deferred.Or_error.t)
  -> unit Deferred.Or_error.t
