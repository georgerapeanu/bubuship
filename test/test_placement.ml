open Core
open Import

let%expect_test "random placement can touch" =
  Random.set_state (Random.State.make [| 0 |]);
  let placement =
    Placement.get_placement
      ~width:10
      ~height:10
      ~ship_lengths:[ 5; 4; 3; 3; 2 ]
      ~can_touch:true
  in
  print_s [%sexp (placement : Message.Ship_placement.t list)];
  [%expect
    {|
    (((len 5) (x 2) (y 4) (dir V)) ((len 4) (x 4) (y 0) (dir V))
     ((len 3) (x 9) (y 2) (dir V)) ((len 3) (x 6) (y 1) (dir V))
     ((len 2) (x 6) (y 8) (dir V)))
    |}]
;;

let%expect_test "random placement cant touch" =
  Random.set_state (Random.State.make [| 0 |]);
  let placement =
    Placement.get_placement
      ~width:10
      ~height:10
      ~ship_lengths:[ 5; 4; 3; 3; 2 ]
      ~can_touch:false
  in
  print_s [%sexp (placement : Message.Ship_placement.t list)];
  [%expect
    {|
    (((len 5) (x 2) (y 4) (dir V)) ((len 4) (x 4) (y 0) (dir V))
     ((len 3) (x 9) (y 2) (dir V)) ((len 3) (x 6) (y 1) (dir V))
     ((len 2) (x 6) (y 8) (dir V)))
    |}]
;;
