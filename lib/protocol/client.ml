open Core
open Async

type t = { write : Message.t -> unit Deferred.t } [@@deriving fields ~getters]

let with_connection ~uri ~game_lobby ~token ~on_connect ~f =
  let%bind.Deferred.Or_error _response, websocket =
    Cohttp_async_websocket.Client.create
      ~force_ssl_overriding_SNI_hostname:(Uri.host uri |> Option.value_exn)
      uri
  in
  let reader, writer = Websocket.pipes websocket in
  let t =
    { write =
        (fun message ->
          [%log.global.info "writing" (message : Message.t)];
          Pipe.write_if_open writer (Message.jsonaf_of_t message |> Jsonaf.to_string))
    }
  in
  let%bind () = on_connect ~client:t in
  let%map () =
    Pipe.iter reader ~f:(fun message ->
      [%log.global.info "Received message" (message : string)];
      Deferred.Or_error.try_with_join (fun () ->
        let jsonaf = Jsonaf.of_string message in
        Deferred.Or_error.try_with_join (fun () ->
          let message = Message.t_of_jsonaf jsonaf in
          f message ~client:t)
        |> Deferred.Or_error.tag_s ~tag:[%message "Errored on json" (jsonaf : Jsonaf.t)])
      |> Deferred.Or_error.tag_s ~tag:[%message "errored on message" (message : string)]
      |> Deferred.Or_error.ok_exn)
  in
  Pipe.close writer |> Or_error.return
;;
