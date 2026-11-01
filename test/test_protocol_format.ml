open Core
open Import
open Jsonaf.Export

let parse_json json_string =
  let message = Jsonaf.of_string json_string |> Message.t_of_jsonaf in
  Message.sexp_of_t message |> print_s;
  message
;;

let generate_json message =
  Message.jsonaf_of_t message |> Jsonaf.to_string_hum |> print_string
;;

let%expect_test "hello" =
  let message =
    parse_json
      {|
      {
        "type": "hello",
                "token": "bsc_xxxxxxxxxxxxxxxxxxxxxxxx",
                "lobby": "K7QM-4XD2-7",
                "accepts": ["classic-10x10"],
                "program": { "name": "my-bot", "version": "0.3.1" },
                "session_id": "optional, see section 6"
      }
    |}
  in
  [%expect
    {|
    (Hello
     ((token bsc_xxxxxxxxxxxxxxxxxxxxxxxx) (lobby K7QM-4XD2-7)
      (accepts (classic-10x10)) (program ((name my-bot) (version 0.3.1)))
      (session_id ("optional, see section 6"))))
    |}];
  generate_json message;
  [%expect
    {|
    {
      "type": "hello",
      "token": "bsc_xxxxxxxxxxxxxxxxxxxxxxxx",
      "lobby": "K7QM-4XD2-7",
      "accepts": [
        "classic-10x10"
      ],
      "program": {
        "name": "my-bot",
        "version": "0.3.1"
      },
      "session_id": "optional, see section 6"
    }
    |}]
;;

let%expect_test "welcome" =
  let message =
    parse_json
      {|
    {
      "type": "welcome",
      "session_id": "s_9b1c...",
      "seat": 1,
      "resumed": false,
      "lobby": "K7QM-4XD2-7",
      "ruleset": {
        "id": "classic-10x10", "width": 10, "height": 10,
        "fleet": [5, 4, 3, 3, 2],
        "touching": false, "extra_turn_on_hit": true,
        "announce_sunk": true, "repeat_shot": "invalid",
        "reveal_on_game_over": true
      },
      "limits": { "place_ms": 10000, "move_ms": 10000, "grace_ms": 60000, "max_strikes": 3 }
    }
    |}
  in
  [%expect
    {|
    (Welcome
     ((session_id s_9b1c...) (seat 1) (resumed false) (lobby K7QM-4XD2-7)
      (ruleset
       ((id classic-10x10) (width 10) (height 10) (fleet (5 4 3 3 2))
        (touching false) (extra_turn_on_hit true) (announce_sunk true)
        (repeat_shot invalid) (reveal_on_game_over true)))
      (limits
       ((place_ms 10000) (move_ms 10000) (grace_ms 60000) (max_strikes 3)))))
    |}];
  generate_json message;
  [%expect
    {|
    {
      "type": "welcome",
      "session_id": "s_9b1c...",
      "seat": 1,
      "resumed": false,
      "lobby": "K7QM-4XD2-7",
      "ruleset": {
        "id": "classic-10x10",
        "width": 10,
        "height": 10,
        "fleet": [
          5,
          4,
          3,
          3,
          2
        ],
        "touching": false,
        "extra_turn_on_hit": true,
        "announce_sunk": true,
        "repeat_shot": "invalid",
        "reveal_on_game_over": true
      },
      "limits": {
        "place_ms": 10000,
        "move_ms": 10000,
        "grace_ms": 60000,
        "max_strikes": 3
      }
    }
    |}]
;;

let%expect_test "place" =
  let message =
    parse_json
      {|
    {
      "type": "place", "rid": 1, "deadline_ms": 10000,
      "game": { "id": "g_8f3a", "index": 1, "total": 100, "first": "you" },
      "opponent": { "name": "Alex", "program": { "name": "x", "version": "1" } },
      "error": null,
      "strikes": 0
    }
    |}
  in
  [%expect
    {|
    (Place
     ((rid 1) (deadline_ms 10000)
      (game ((id g_8f3a) (index 1) (total 100) (first (you))))
      (opponent ((name Alex) (program (((name x) (version 1)))))) (strikes 0)
      (error ())))
    |}];
  generate_json message;
  [%expect
    {|
    {
      "type": "place",
      "rid": 1,
      "deadline_ms": 10000,
      "game": {
        "id": "g_8f3a",
        "index": 1,
        "total": 100,
        "first": "you"
      },
      "opponent": {
        "name": "Alex",
        "program": {
          "name": "x",
          "version": "1"
        }
      },
      "strikes": 0,
      "error": null
    }
    |}]
;;

let%expect_test "placement" =
  let message =
    parse_json
      {|
    {
      "type": "placement", "rid": 1,
      "ships": [
        { "len": 5, "x": 0, "y": 0, "dir": "h" },
        { "len": 4, "x": 0, "y": 2, "dir": "h" }
      ]
    }
    |}
  in
  [%expect
    {|
    (Placement
     ((rid 1)
      (ships (((len 5) (x 0) (y 0) (dir H)) ((len 4) (x 0) (y 2) (dir H))))))
    |}];
  generate_json message;
  [%expect
    {|
    {
      "type": "placement",
      "rid": 1,
      "ships": [
        {
          "len": 5,
          "x": 0,
          "y": 0,
          "dir": "h"
        },
        {
          "len": 4,
          "x": 0,
          "y": 2,
          "dir": "h"
        }
      ]
    }
    |}]
;;

let%expect_test "turn" =
  let message =
    parse_json
      {|
    {
      "type": "turn", "rid": 7, "deadline_ms": 10000,
      "game": { "id": "g_8f3a", "index": 1, "total": 100, "first": "you" },
      "you": [
        "SSSSS.....",
        ".o........",
        "SSSS.o....",
        "..........",
        "SSS..SSS..",
        "..........",
        "........xx",
        "..........",
        "..........",
        ".........."
      ],
      "enemy": [
        "..........",
        "...o......",
        "..........",
        "...x......",
        "...x......",
        "..........",
        "......o...",
        "..........",
        "..........",
        ".........."
      ],
      "enemy_remaining": [5, 4, 3, 2],
      "last_shot": { "x": 3, "y": 4, "result": "hit" },
      "incoming": [ { "x": 1, "y": 1, "result": "miss" } ],
      "error": null,
      "strikes": 0
    }
    |}
  in
  [%expect
    {|
    (Turn
     ((rid 7) (deadline_ms 10000)
      (game ((id g_8f3a) (index 1) (total 100) (first (you))))
      (you
       ((S S S S S Empty Empty Empty Empty Empty)
        (Empty Miss Empty Empty Empty Empty Empty Empty Empty Empty)
        (S S S S Empty Miss Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (S S S Empty Empty S S S Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Hit Hit)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)))
      (opponent
       ((Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Miss Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Hit Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Hit Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Miss Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)))
      (opponent_remaining (5 4 3 2)) (last_shot (((x 3) (y 4) (result Hit))))
      (incoming (((x 1) (y 1) (result Miss)))) (error ()) (strikes 0)))
    |}];
  generate_json message;
  [%expect
    {|
    {
      "type": "turn",
      "rid": 7,
      "deadline_ms": 10000,
      "game": {
        "id": "g_8f3a",
        "index": 1,
        "total": 100,
        "first": "you"
      },
      "you": [
        "SSSSS.....",
        ".o........",
        "SSSS.o....",
        "..........",
        "SSS..SSS..",
        "..........",
        "........xx",
        "..........",
        "..........",
        ".........."
      ],
      "enemy": [
        "..........",
        "...o......",
        "..........",
        "...x......",
        "...x......",
        "..........",
        "......o...",
        "..........",
        "..........",
        ".........."
      ],
      "enemy_remaining": [
        5,
        4,
        3,
        2
      ],
      "last_shot": {
        "x": 3,
        "y": 4,
        "result": "hit",
        "len": null
      },
      "incoming": [
        {
          "x": 1,
          "y": 1,
          "result": "miss",
          "len": null
        }
      ],
      "error": null,
      "strikes": 0
    }
    |}]
;;

let%expect_test "fire" =
  let message =
    parse_json
      {|
    { "type": "fire", "rid": 7, "x": 3, "y": 4 }
    |}
  in
  [%expect {| (Fire ((rid 7) (x 3) (y 4))) |}];
  generate_json message;
  [%expect
    {|
    {
      "type": "fire",
      "rid": 7,
      "x": 3,
      "y": 4
    }
    |}]
;;

let%expect_test "game_over" =
  let message =
    parse_json
      {|
    {
      "type": "game_over",
      "game": { "id": "g_8f3a", "index": 1, "total": 100, "first": "you" },
      "winner": "you",
      "reason": "sunk",
      "last_shot": { "x": 6, "y": 6, "result": "sunk", "len": 2 },
      "incoming": [],
      "series": { "score": { "you": 1, "opponent": 0 }, "done": false },
      "reveal": {
        "opponent_ships": [ { "len": 5, "x": 0, "y": 0, "dir": "h" } ],
        "opponent_shots": [ { "x": 1, "y": 1, "result": "miss" } ]
      }
    }
    |}
  in
  [%expect
    {|
    (Game_over
     ((game ((id g_8f3a) (index 1) (total 100) (first (you)))) (winner you)
      (reason sunk) (last_shot (((x 6) (y 6) (result (Sunk 2))))) (incoming ())
      (series
       (Object
        ((score (Object ((you (Number 1)) (opponent (Number 0))))) (done False))))
      (reveal
       (Object
        ((opponent_ships
          (Array
           ((Object
             ((len (Number 5)) (x (Number 0)) (y (Number 0)) (dir (String h)))))))
         (opponent_shots
          (Array
           ((Object ((x (Number 1)) (y (Number 1)) (result (String miss))))))))))))
    |}];
  generate_json message;
  [%expect
    {|
    {
      "type": "game_over",
      "game": {
        "id": "g_8f3a",
        "index": 1,
        "total": 100,
        "first": "you"
      },
      "winner": "you",
      "reason": "sunk",
      "last_shot": {
        "x": 6,
        "y": 6,
        "result": "sunk",
        "len": 2
      },
      "incoming": [],
      "series": {
        "score": {
          "you": 1,
          "opponent": 0
        },
        "done": false
      },
      "reveal": {
        "opponent_ships": [
          {
            "len": 5,
            "x": 0,
            "y": 0,
            "dir": "h"
          }
        ],
        "opponent_shots": [
          {
            "x": 1,
            "y": 1,
            "result": "miss"
          }
        ]
      }
    }
    |}]
;;

let%expect_test "reset" =
  let message =
    parse_json
      {|
    { "type": "reset", "reason": "restart_game" }
    |}
  in
  [%expect {| (Reset ((reason restart_game))) |}];
  generate_json message;
  [%expect
    {|
    {
      "type": "reset",
      "reason": "restart_game"
    }
    |}]
;;

let%expect_test "error" =
  let message =
    parse_json
      {|
    { "type": "error", "code": "bad_json", "message": "human readable", "fatal": false }
    |}
  in
  [%expect {| (Error ((code bad_json) (message "human readable") (fatal false))) |}];
  generate_json message;
  [%expect
    {|
    {
      "type": "error",
      "code": "bad_json",
      "message": "human readable",
      "fatal": false
    }
    |}]
;;

let%expect_test "turn 2" =
  let message =
    parse_json
      {|
      {"type":"turn","rid":24,"deadline_ms":10000,"game":{"id":"g_gy1re6kx9g23","index":1,"total":1,"first":"you"},"you":["........SS","..........","..SSSSS...","..........","..........","..SSS.....","S.........","S...S.....","S...S.....","S...S....."],"enemy":["..........","..........","..........","..........","..........","..........","..........","..........","..........",".........."],"enemy_remaining":[5,4,3,3,2],"last_shot":null,"incoming":[],"error":null,"strikes":0}
    |}
  in
  [%expect {|
    (Turn
     ((rid 24) (deadline_ms 10000)
      (game ((id g_gy1re6kx9g23) (index 1) (total 1) (first (you))))
      (you
       ((Empty Empty Empty Empty Empty Empty Empty Empty S S)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty S S S S S Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty S S S Empty Empty Empty Empty Empty)
        (S Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (S Empty Empty Empty S Empty Empty Empty Empty Empty)
        (S Empty Empty Empty S Empty Empty Empty Empty Empty)
        (S Empty Empty Empty S Empty Empty Empty Empty Empty)))
      (opponent
       ((Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)
        (Empty Empty Empty Empty Empty Empty Empty Empty Empty Empty)))
      (opponent_remaining (5 4 3 3 2)) (last_shot ()) (incoming ()) (error ())
      (strikes 0)))
    |}];
  generate_json message;
  [%expect {|
    {
      "type": "turn",
      "rid": 24,
      "deadline_ms": 10000,
      "game": {
        "id": "g_gy1re6kx9g23",
        "index": 1,
        "total": 1,
        "first": "you"
      },
      "you": [
        "........SS",
        "..........",
        "..SSSSS...",
        "..........",
        "..........",
        "..SSS.....",
        "S.........",
        "S...S.....",
        "S...S.....",
        "S...S....."
      ],
      "enemy": [
        "..........",
        "..........",
        "..........",
        "..........",
        "..........",
        "..........",
        "..........",
        "..........",
        "..........",
        ".........."
      ],
      "enemy_remaining": [
        5,
        4,
        3,
        3,
        2
      ],
      "last_shot": null,
      "incoming": [],
      "error": null,
      "strikes": 0
    }
    |}]
;;
