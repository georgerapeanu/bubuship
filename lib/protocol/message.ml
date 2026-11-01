open! Core
open! Async
open Jsonaf.Export

module V1 = struct
  module Program = struct
    type t =
      { name : string
      ; version : string
      }
    [@@deriving jsonaf, sexp]
  end

  module Ruleset = struct
    type t =
      { id : string
      ; width : int
      ; height : int
      ; fleet : int list
      ; touching : bool
      ; extra_turn_on_hit : bool
      ; announce_sunk : bool
      ; repeat_shot : string
      ; reveal_on_game_over : bool
      }
    [@@deriving jsonaf, sexp]
  end

  module Limits = struct
    type t =
      { place_ms : int
      ; move_ms : int
      ; grace_ms : int
      ; max_strikes : int
      }
    [@@deriving jsonaf, sexp]
  end

  module Player = struct
    type t =
      | You
      | Opponent
    [@@deriving sexp]

    let t_of_jsonaf jsonaf =
      match jsonaf with
      | `String "you" -> You
      | `String _ -> Opponent
      | _ -> failwith "Unexpected player"
    ;;

    let jsonaf_of_t = function
      | You -> `String "you"
      | Opponent -> `Sring "opponent"
    ;;
  end

  module Opponent = struct
    type t =
      { name : string
      ; program : Program.t option [@default None]
      }
    [@@deriving jsonaf, sexp]
  end

  module Game = struct
    type t =
      { id : string
      ; index : int
      ; total : int
      ; first : string option [@default None]
      }
    [@@deriving jsonaf, sexp]
  end

  module Hello_record = struct
    type t =
      { token : string
      ; lobby : string
      ; accepts : string list
      ; program : Program.t
      ; session_id : string option [@jsonaf.option]
      }
    [@@deriving jsonaf, sexp]
  end

  module Welcome_record = struct
    type t =
      { session_id : string
      ; seat : int
      ; resumed : bool
      ; lobby : string
      ; ruleset : Ruleset.t
      ; limits : Limits.t
      }
    [@@deriving jsonaf, sexp]
  end

  module Place_record = struct
    type t =
      { rid : int
      ; deadline_ms : int
      ; game : Game.t
      ; opponent : Opponent.t
      ; strikes : int
      ; error : Jsonaf.t option [@default None]
      }
    [@@deriving jsonaf, sexp]
  end

  module Ship_orientation = struct
    type t =
      | H
      | V
    [@@deriving sexp]

    let t_of_jsonaf jsonaf =
      match jsonaf with
      | `String "h" -> H
      | `String "v" -> V
      | _ -> failwith "Unexpected orientation"
    ;;

    let jsonaf_of_t = function
      | H -> `String "h"
      | V -> `String "v"
    ;;
  end

  module Ship_placement = struct
    type t =
      { len : int
      ; x : int
      ; y : int
      ; dir : Ship_orientation.t
      }
    [@@deriving jsonaf, sexp]
  end

  module Placement_record = struct
    type t =
      { rid : int
      ; ships : Ship_placement.t list
      }
    [@@deriving jsonaf, sexp]
  end

  module Board = struct
    module Format = struct
      type t = string array [@@deriving jsonaf, sexp]
    end

    module Make (Cell_state : sig
        type t [@@deriving jsonaf, string, sexp]
      end) =
    struct
      type t = Cell_state.t array array [@@deriving sexp]

      let height = Array.length
      let width t = Array.length t.(0)

      let format_of_t t =
        Array.map t ~f:(fun row ->
          Array.map row ~f:Cell_state.to_string |> String.concat_array ~sep:"")
      ;;

      let t_of_format format =
        Array.map format ~f:(fun row ->
          String.to_array row
          |> Array.map ~f:(fun c -> String.of_char c |> Cell_state.of_string))
      ;;

      let jsonaf_of_t t = format_of_t t |> Format.jsonaf_of_t
      let t_of_jsonaf jsonaf = Format.t_of_jsonaf jsonaf |> t_of_format
    end

    module Cell_state = struct
      module Self = struct
        type t =
          | S
          | Hit
          | Miss
          | Empty
        [@@deriving equal, sexp]

        let of_string = function
          | "S" -> S
          | "x" -> Hit
          | "o" -> Miss
          | "." -> Empty
          | _ -> failwith "Unexpected self board entry"
        ;;

        let to_string = function
          | S -> "S"
          | Hit -> "x"
          | Miss -> "o"
          | Empty -> "."
        ;;

        let t_of_jsonaf = function
          | `String "S" -> S
          | `String "x" -> Hit
          | `String "o" -> Miss
          | `String "." -> Empty
          | _ -> failwith "Unexpected self board entry"
        ;;

        let jsonaf_of_t = function
          | S -> `String "S"
          | Hit -> `String "x"
          | Miss -> `String "o"
          | Empty -> `String "."
        ;;
      end

      module Opponent = struct
        type t =
          | Hit
          | Sunk
          | Miss
          | Empty
        [@@deriving equal, sexp]

        let of_string = function
          | "x" -> Hit
          | "#" -> Sunk
          | "o" -> Miss
          | "." -> Empty
          | _ -> failwith "Unexpected opponent board entry"
        ;;

        let to_string = function
          | Hit -> "x"
          | Sunk -> "#"
          | Miss -> "o"
          | Empty -> "."
        ;;

        let t_of_jsonaf = function
          | `String "x" -> Hit
          | `String "#" -> Sunk
          | `String "o" -> Miss
          | `String "." -> Empty
          | _ -> failwith "Unexpected opponent board entry"
        ;;

        let jsonaf_of_t = function
          | Hit -> `String "x"
          | Sunk -> `String "#"
          | Miss -> `String "o"
          | Empty -> `String "."
        ;;
      end
    end

    module Self = struct
      include Make (Cell_state.Self)
      module Cell_state = Cell_state.Self
    end

    module Opponent = struct
      include Make (Cell_state.Opponent)
      module Cell_state = Cell_state.Opponent
    end
  end

  module Shot = struct
    module Shot_result = struct
      type t =
        | Hit
        | Miss
        | Sunk of int
      [@@deriving sexp]
    end

    module Format = struct
      type t =
        { x : int
        ; y : int
        ; result : string
        ; len : int option [@default None]
        }
      [@@deriving jsonaf, sexp]
    end

    type t =
      { x : int
      ; y : int
      ; result : Shot_result.t
      }
    [@@deriving sexp]

    let format_of_t { x; y; result } =
      let format_result, format_len =
        match result with
        | Hit -> "hit", None
        | Miss -> "miss", None
        | Sunk len -> "sunk", Some len
      in
      { Format.x; y; result = format_result; len = format_len }
    ;;

    let t_of_format { Format.x; y; result; len } =
      let result =
        match result, len with
        | "hit", None -> Shot_result.Hit
        | "miss", None -> Miss
        | "sunk", Some len -> Sunk len
        | _ -> failwith "Unexpected result x len combo for shot"
      in
      { x; y; result }
    ;;

    let t_of_jsonaf jsonaf = Format.t_of_jsonaf jsonaf |> t_of_format
    let jsonaf_of_t t = format_of_t t |> Format.jsonaf_of_t
  end

  module Turn_record = struct
    type t =
      { rid : int
      ; deadline_ms : int
      ; game : Game.t
      ; you : Board.Self.t
      ; opponent : Board.Opponent.t [@key "enemy"]
      ; opponent_remaining : int list [@key "enemy_remaining"]
      ; last_shot : Shot.t option [@default None]
      ; incoming : Shot.t list
      ; error : Jsonaf.t option
      ; strikes : int
      }
    [@@deriving jsonaf, sexp]
  end

  module Fire_record = struct
    type t =
      { rid : int
      ; x : int
      ; y : int
      }
    [@@deriving jsonaf, sexp]
  end

  module Game_over_record = struct
    type t =
      { game : Game.t
      ; winner : string
      ; reason : string
      ; last_shot : Shot.t option [@default None]
      ; incoming : Shot.t list
      ; series : Jsonaf.t
      ; reveal : Jsonaf.t
      }
    [@@deriving jsonaf, sexp]
  end

  module Reset_record = struct
    type t = { reason : string } [@@deriving jsonaf, sexp]
  end

  module Error_record = struct
    type t =
      { code : string
      ; message : string
      ; fatal : bool
      }
    [@@deriving jsonaf, sexp]
  end

  module Format = struct
    type t =
      { type_ : string
      ; body : Jsonaf.t
      }

    let jsonaf_of_t { type_; body } =
      match body with
      | `Object entries -> `Object (("type", `String type_) :: entries)
      | _ ->
        raise_s
          [%message "Unexpected jsonaf message body shape was produced" (body : Jsonaf.t)]
    ;;

    let t_of_jsonaf jsonaf =
      match jsonaf with
      | `Object entries ->
        (match List.Assoc.find entries ~equal:String.equal "type" with
         | None -> failwith "Received message had no type"
         | Some (`String type_) ->
           { type_
           ; body =
               `Object
                 (List.filter entries ~f:(fun (key, _) -> not (String.equal "type" key)))
           }
         | Some _ -> failwith "Received message type was more complex than expected")
      | _ -> failwith "Unexpected json message is not an object"
    ;;
  end

  type t =
    | Hello of Hello_record.t
    | Welcome of Welcome_record.t
    | Place of Place_record.t
    | Placement of Placement_record.t
    | Turn of Turn_record.t
    | Fire of Fire_record.t
    | Game_over of Game_over_record.t
    | Reset of Reset_record.t
    | Error of Error_record.t
  [@@deriving sexp]

  let format_of_t = function
    | Hello hello -> { Format.type_ = "hello"; body = Hello_record.jsonaf_of_t hello }
    | Welcome welcome ->
      { Format.type_ = "welcome"; body = Welcome_record.jsonaf_of_t welcome }
    | Place place -> { Format.type_ = "place"; body = Place_record.jsonaf_of_t place }
    | Placement placement ->
      { Format.type_ = "placement"; body = Placement_record.jsonaf_of_t placement }
    | Turn turn -> { Format.type_ = "turn"; body = Turn_record.jsonaf_of_t turn }
    | Fire fire -> { Format.type_ = "fire"; body = Fire_record.jsonaf_of_t fire }
    | Game_over game_over ->
      { Format.type_ = "game_over"; body = Game_over_record.jsonaf_of_t game_over }
    | Reset reset -> { Format.type_ = "reset"; body = Reset_record.jsonaf_of_t reset }
    | Error error -> { Format.type_ = "error"; body = Error_record.jsonaf_of_t error }
  ;;

  let t_of_format { Format.type_; body } =
    match type_ with
    | "hello" -> Hello (Hello_record.t_of_jsonaf body)
    | "welcome" -> Welcome (Welcome_record.t_of_jsonaf body)
    | "place" -> Place (Place_record.t_of_jsonaf body)
    | "placement" -> Placement (Placement_record.t_of_jsonaf body)
    | "turn" -> Turn (Turn_record.t_of_jsonaf body)
    | "fire" -> Fire (Fire_record.t_of_jsonaf body)
    | "game_over" -> Game_over (Game_over_record.t_of_jsonaf body)
    | "reset" -> Reset (Reset_record.t_of_jsonaf body)
    | "error" -> Error (Error_record.t_of_jsonaf body)
    | _ -> failwith "Unexpected format"
  ;;

  let t_of_jsonaf jsonaf = Format.t_of_jsonaf jsonaf |> t_of_format
  let jsonaf_of_t t = format_of_t t |> Format.jsonaf_of_t
end

module Latest = V1
include Latest

module For_testing = struct
  include Latest
end
