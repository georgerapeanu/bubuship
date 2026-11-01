open Core
open Async
open Import

let get_shot ~opponent_board =
  let possible_shot =
    Array.filter_mapi opponent_board ~f:(fun y row ->
      let possible_shot =
        Array.filter_mapi row ~f:(fun x cell ->
          Option.some_if (Message.Board.Opponent.Cell_state.equal cell Empty) (~x, ~y))
      in
      if Array.length possible_shot = 0 then None else Some possible_shot.(0))
  in
  if Array.length possible_shot = 0 then None else Some possible_shot.(0)
;;
