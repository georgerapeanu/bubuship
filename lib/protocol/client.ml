open Core
open Async

type write_fn = Message.t -> unit Deferred.t

let with_connection ~uri ~game_lobby ~token ~on_connect ~f =
  let%bind.Deferred.Or_error _response, websocket =
    Cohttp_async_websocket.Client.create
      ~force_ssl_overriding_SNI_hostname:(Uri.host uri |> Option.value_exn)
      uri
  in
  let reader, writer = Websocket.pipes websocket in
  let write message =
    print_endline (Message.jsonaf_of_t message |> Jsonaf.to_string);
    Pipe.write_if_open writer (Message.jsonaf_of_t message |> Jsonaf.to_string)
  in
  let%bind () = on_connect ~write in
  let%map () =
    Pipe.iter reader ~f:(fun message ->
      [%log.global.info "Received message" (message : string)];
      Deferred.Or_error.try_with_join (fun () ->
        let jsonaf = Jsonaf.of_string message in
        Deferred.Or_error.try_with_join (fun () ->
          let message = Message.t_of_jsonaf jsonaf in
          f message ~write)
        |> Deferred.Or_error.tag_s ~tag:[%message "Errored on json" (jsonaf : Jsonaf.t)])
      |> Deferred.Or_error.tag_s ~tag:[%message "errored on message" (message : string)]
      |> Deferred.Or_error.ok_exn)
  in
  Pipe.close writer |> Or_error.return
;;
