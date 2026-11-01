open Core
open Async
open Import

val get_placement
  :  height:int
  -> width:int
  -> ship_lengths:int list
  -> can_touch:bool
  -> Message.Ship_placement.t list
