open Core
open Async
open Import

val get_shot : opponent_board:Message.Board.Opponent.t -> (x:int * y:int) option
