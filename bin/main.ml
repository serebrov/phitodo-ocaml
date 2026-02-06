(** Main entry point for phitodo TUI *)

open Phitodo

(** Get terminal size using stty *)
let terminal_size () =
  try
    let ic = Unix.open_process_in "stty size </dev/tty 2>/dev/null" in
    let line = input_line ic in
    let _ = Unix.close_process_in ic in
    match String.split_on_char ' ' line with
    | [rows; cols] -> (int_of_string cols, int_of_string rows)
    | _ -> (80, 24)
  with _ -> (80, 24)

(** Minttea application model *)
type model = {
  app : App.t;
  width : int;
  height : int;
}

(** Initialize the application *)
let init_model () =
  (* Get terminal size *)
  let width, height = terminal_size () in

  (* Load configuration *)
  let config = Config.load () in

  (* Create application state *)
  let app = App.create config in

  (* Load initial data *)
  let app = App.refresh app in

  (* Update sidebar focus *)
  let sidebar = Sidebar.set_focused app.App.sidebar true in
  let app = { app with sidebar } in

  { app; width; height }

(** Initialize function for Minttea *)
let init _model =
  Minttea.Command.Seq [
    Minttea.Command.Enter_alt_screen;
    Minttea.Command.Hide_cursor;
  ]

(** Update function for Minttea *)
let update event model =
  match event with
  | Minttea.Event.KeyDown key ->
      let app, msg = Update.update model.app key in
      (match msg with
      | Some Update.Quit ->
          App.close model.app;
          ({ model with app }, Minttea.Command.Seq [
            Minttea.Command.Show_cursor;
            Minttea.Command.Exit_alt_screen;
            Minttea.Command.Quit;
          ])
      | _ ->
          ({ model with app }, Minttea.Command.Noop))

  | Minttea.Event.Timer _ ->
      let app = App.process_async_messages model.app in
      ({ model with app }, Minttea.Command.Noop)

  | Minttea.Event.Frame _ ->
      (* Update terminal size on each frame *)
      let width, height = terminal_size () in
      ({ model with width; height }, Minttea.Command.Noop)

  | Minttea.Event.Custom _ ->
      (model, Minttea.Command.Noop)

(** View function for Minttea *)
let view model =
  View.render model.app model.width model.height

(** Main program *)
let () =
  (* Initialize random number generator for UUIDs *)
  Random.self_init ();

  (* Run inside Riot runtime *)
  Riot.run @@ fun () ->
    (* Create initial model *)
    let initial_model = init_model () in

    (* Run the Minttea application *)
    Minttea.app ~init ~update ~view ()
    |> Minttea.run ~initial_model
