open Core
open Async

type t

val write : t -> Message.t -> unit Deferred.t

val with_connection
  :  uri:Uri.t
  -> game_lobby:string
  -> token:string
  -> on_connect:(client:t -> unit Deferred.t)
  -> f:(Message.t -> client:t -> unit Deferred.Or_error.t)
  -> unit Deferred.Or_error.t
