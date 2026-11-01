open Core
open Async

let command =
  Command.async_or_error
    ~summary:"plays one round of bubuships"
    (
      let open Command.Param in
      let%map_open.Command token = flag "-token" (required string) ~doc:"temp" and game_lobby = flag "-lobby" (required string) ~doc:"temp" and uri= flag "-uri" (required string) ~doc:"temp" in
      fun () -> (
          let uri = Uri.of_string uri in
          Bubuship_protocol.Client.with_connection ~uri ~token ~game_lobby ~on_connect:(fun ~write ->
              let%map () = write (Hello {token; lobby = game_lobby; accepts = ["classic-10x10"]; program={name="my-bot"; version="0.0.0"};session_id = None}) in
              ()

            ) ~f:(fun message ~write ->
              Deferred.Or_error.return ()
            )
        )
    )

let () = Command_unix.run command
