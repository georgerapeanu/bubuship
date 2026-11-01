include struct
  open Bubuship_protocol
  module Client = Client
  module Message = Message
end

include struct
  open Bubuship_strategy
  module Placement = Placement
  module Turn = Turn
end
