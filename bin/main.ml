open Core
open Async
open Import

let command =
  Command.async_or_error
    ~summary:"plays one round of bubuships"
    (let open Command.Param in
     let%map_open.Command token = flag "-token" (required string) ~doc:"temp"
     and game_lobby = flag "-lobby" (required string) ~doc:"temp"
     and uri = flag "-uri" (required string) ~doc:"temp" in
     fun () ->
       let uri = Uri.of_string uri in
       let ruleset = ref None in
       Client.with_connection
         ~uri
         ~token
         ~game_lobby
         ~on_connect:(fun ~client ->
           let%map () =
             Client.write
               client
               (Message.Hello
                  { token
                  ; lobby = game_lobby
                  ; accepts = [ "classic-10x10" ]
                  ; program = { name = "my-bot"; version = "0.0.0" }
                  ; session_id = None
                  })
           in
           ())
         ~f:(fun message ~client ->
           match message with
           | Welcome welcome ->
             ruleset := Some welcome.ruleset;
             Deferred.Or_error.return ()
           | Place { rid; _ } ->
             let ruleset = Option.value_exn !ruleset in
             let placement =
               Placement.get_placement
                 ~height:ruleset.height
                 ~width:ruleset.width
                 ~ship_lengths:ruleset.fleet
                 ~can_touch:ruleset.touching
             in
             Message.Placement { rid; ships = placement }
             |> Client.write client
             |> Deferred.map ~f:Or_error.return
           | Turn { rid; opponent; _ } ->
             let ~x, ~y = Turn.get_shot ~opponent_board:opponent |> Option.value_exn in
             Message.Fire { x; y; rid }
             |> Client.write client
             |> Deferred.map ~f:Or_error.return
           | _ -> Deferred.Or_error.return ()))
;;

let () = Command_unix.run command
