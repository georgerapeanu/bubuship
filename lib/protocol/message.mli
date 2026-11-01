open Core

module Program : sig
  type t =
    { name : string
    ; version : string
    }
  [@@deriving sexp]
end

module Ruleset : sig
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
  [@@deriving sexp]
end

module Limits : sig
  type t =
    { place_ms : int
    ; move_ms : int
    ; grace_ms : int
    ; max_strikes : int
    }
  [@@deriving sexp]
end

module Player : sig
  type t =
    | You
    | Opponent
  [@@deriving sexp]
end

module Opponent : sig
  type t =
    { name : string
    ; program : Program.t option
    }
  [@@deriving sexp]
end

module Game : sig
  type t =
    { id : string
    ; index : int
    ; total : int
    ; first : string
    }
  [@@deriving sexp]
end

module Hello_record : sig
  type t =
    { token : string
    ; lobby : string
    ; accepts : string list
    ; program : Program.t
    ; session_id : string option
    }
  [@@deriving sexp]
end

module Welcome_record : sig
  type t =
    { session_id : string
    ; seat : int
    ; resumed : bool
    ; lobby : string
    ; ruleset : Ruleset.t
    ; limits : Limits.t
    }
  [@@deriving sexp]
end

module Place_record : sig
  type t =
    { rid : int
    ; deadline_ms : int
    ; game : Game.t
    ; opponent : Opponent.t
    ; strikes : int
    ; error : Jsonaf.t option
    }
  [@@deriving sexp]
end

module Ship_orientation : sig
  type t =
    | H
    | V
  [@@deriving sexp]
end

module Ship_placement : sig
  type t =
    { len : int
    ; x : int
    ; y : int
    ; dir : Ship_orientation.t
    }
  [@@deriving sexp]
end

module Placement_record : sig
  type t =
    { rid : int
    ; ships : Ship_placement.t list
    }
  [@@deriving sexp]
end

module Board : sig
  module Self : sig
    module Cell_state : sig
      type t =
        | S
        | Hit
        | Miss
        | Empty
      [@@deriving equal, sexp]
    end

    type t = Cell_state.t array array

    val height : 'a array -> int
    val width : 'a array array -> int
  end

  module Opponent : sig
    module Cell_state : sig
      type t =
        | Hit
        | Sunk
        | Miss
        | Empty
      [@@deriving equal, sexp]
    end

    type t = Cell_state.t array array

    val height : 'a array -> int
    val width : 'a array array -> int
  end
end

module Shot : sig
  module Shot_result : sig
    type t =
      | Hit
      | Miss
      | Sunk of int
    [@@deriving sexp]
  end

  type t =
    { x : int
    ; y : int
    ; result : Shot_result.t
    }
  [@@deriving sexp]
end

module Turn_record : sig
  type t =
    { rid : int
    ; deadline_ms : int
    ; game : Game.t
    ; you : Board.Self.t
    ; opponent : Board.Opponent.t
    ; opponent_remaining : int list
    ; last_shot : Shot.t option
    ; incoming : Shot.t list
    ; error : Jsonaf.t option
    ; strikes : int
    }
  [@@deriving sexp]
end

module Fire_record : sig
  type t =
    { rid : int
    ; x : int
    ; y : int
    }
  [@@deriving sexp]
end

module Game_over_record : sig
  type t =
    { game : Game.t
    ; winner : string
    ; reason : string
    ; last_shot : Shot.t option
    ; incoming : Shot.t list
    ; series : Jsonaf.t
    ; reveal : Jsonaf.t
    }
  [@@deriving sexp]
end

module Reset_record : sig
  type t = { reason : string } [@@deriving sexp]
end

module Error_record : sig
  type t =
    { code : string
    ; message : string
    ; fatal : bool
    }
  [@@deriving sexp]
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
[@@deriving jsonaf, sexp]
