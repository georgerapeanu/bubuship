open Core
open Async
open Import

let get_placement ~height ~width ~ship_lengths ~can_touch =
  let available_for_ship =
    Array.init height ~f:(fun _ ->
      Array.init width ~f:(fun _ -> Message.Board.Self.Cell_state.Empty))
  in
  List.map ship_lengths ~f:(fun length ->
    let rec get_placement () =
      let x = Random.int_incl 0 (height - 1) in
      let y = Random.int_incl 0 (width - 1) in
      let is_vertical = Random.bool () in
      let is_valid =
        List.init length ~f:(fun i ->
          match is_vertical with
          | true -> x, y + i
          | false -> x + i, y)
        |> List.for_all ~f:(fun (x, y) ->
          x < width
          && y < height
          && Message.Board.Self.Cell_state.equal available_for_ship.(y).(x) Empty)
      in
      if is_valid
      then
        { Message.Ship_placement.x
        ; y
        ; dir = (if is_vertical then V else H)
        ; len = length
        }
      else get_placement ()
    in
    let placement = get_placement () in
    List.init placement.len ~f:(fun i ->
      match placement.dir with
      | V -> placement.x, placement.y + i
      | H -> placement.x + i, placement.y)
    |> List.concat_map ~f:(fun (x, y) ->
      let%bind.List dx = [ -1; 0; 1 ] in
      let%bind.List dy = [ -1; 0; 1 ] in
      if x + dx >= 0 && x + dx < height && y + dy >= 0 && y + dy < width
      then [ x + dx, y + dy ]
      else [])
    |> List.iter ~f:(fun (x, y) -> available_for_ship.(y).(x) <- S);
    placement)
;;
