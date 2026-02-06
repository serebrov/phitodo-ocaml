(** View composition / Render function for TEA pattern *)

(** Layout constants *)
let sidebar_width = 25
let min_list_width = 40
let min_detail_width = 30

(** Render status bar *)
let render_status_bar app width =
  let view_name =
    match app.App.current_view with
    | App.Inbox -> "Inbox"
    | App.Today -> "Today"
    | App.Upcoming -> "Upcoming"
    | App.Anytime -> "Anytime"
    | App.Completed -> "Completed"
    | App.Review -> "Review"
    | App.GitHub -> "GitHub"
    | App.Toggl -> "Toggl"
    | App.SettingsView -> "Settings"
    | App.ProjectView _ -> "Project"
    | App.TagView _ -> "Tag"
  in
  let focus_str =
    match app.App.focus with
    | App.Sidebar -> "Sidebar"
    | App.List -> "List"
    | App.Detail -> "Detail"
  in
  let mode_str =
    match app.App.mode with
    | App.Normal -> ""
    | App.TaskForm -> " [EDIT]"
    | App.Confirm -> " [CONFIRM]"
    | App.Settings -> " [SETTINGS]"
    | App.Input -> " [INPUT]"
  in
  let left = Printf.sprintf " %s | %s%s" view_name focus_str mode_str in
  let right = " q:Quit ?:Help " in
  let padding = width - String.length left - String.length right in
  let padding = max 0 padding in
  Theme.Color.bg_primary ^ Theme.Color.white ^
  left ^ String.make padding ' ' ^ right ^
  Theme.Color.reset

(** Combine lines side by side *)
let combine_columns columns =
  let max_height =
    List.fold_left (fun acc (_, lines) -> max acc (List.length lines)) 0 columns
  in
  let padded_columns =
    List.map
      (fun (width, lines) ->
        let padded =
          List.map (fun line -> Theme.pad_right width line) lines
        in
        let extra = max_height - List.length padded in
        if extra > 0 then
          padded @ List.init extra (fun _ -> String.make width ' ')
        else padded)
      columns
  in
  List.init max_height (fun i ->
    String.concat "" (List.map (fun col -> List.nth col i) padded_columns))

(** Render the main content area *)
let render_content app content_width _content_height =
  match app.App.current_view with
  | App.GitHub ->
      Github_view.render app.App.github_view content_width
  | App.Toggl ->
      Toggl_view.render app.App.toggl_view content_width
  | App.SettingsView ->
      Settings_view.render app.App.settings_view content_width
  | _ ->
      (* Task views with list and detail panes *)
      let list_width = content_width * 60 / 100 in
      let detail_width = content_width - list_width in
      let list_lines, detail_lines =
        match app.App.current_view with
        | App.Inbox -> Inbox_view.render app.App.inbox_view list_width detail_width
        | App.Today -> Today_view.render app.App.today_view list_width detail_width
        | App.Upcoming -> Upcoming_view.render app.App.upcoming_view list_width detail_width
        | App.Anytime -> Anytime_view.render app.App.anytime_view list_width detail_width
        | App.Completed -> Completed_view.render app.App.completed_view list_width detail_width
        | App.Review -> Review_view.render app.App.review_view list_width detail_width
        | App.ProjectView _ -> Project_view.render app.App.project_view list_width detail_width
        | App.TagView _ -> Tag_view.render app.App.tag_view list_width detail_width
        | _ -> ([], [])
      in
      combine_columns [(list_width, list_lines); (detail_width, detail_lines)]

(** Main render function *)
let render app width height =
  let lines = ref [] in

  (* Calculate layout *)
  let content_width = width - sidebar_width in
  let content_height = height - 2 in  (* Reserve for status bar and padding *)

  (* Render sidebar *)
  let sidebar_lines = Sidebar.render app.App.sidebar sidebar_width in

  (* Render content *)
  let content_lines = render_content app content_width content_height in

  (* Combine sidebar and content *)
  let main_lines = combine_columns [
    (sidebar_width, sidebar_lines);
    (content_width, content_lines);
  ] in

  lines := main_lines;

  (* Render task form modal if active *)
  (match app.App.task_form with
  | Some form ->
      let form_width = min 60 (width - 10) in
      let form_lines = Task_form.render form form_width in
      let form_height = List.length form_lines in
      let start_y = (height - form_height) / 2 in
      let start_x = (width - form_width) / 2 in
      (* Overlay form on top of main content *)
      lines :=
        List.mapi
          (fun i line ->
            if i >= start_y && i < start_y + form_height then
              let form_line = List.nth form_lines (i - start_y) in
              let before = String.sub line 0 (min start_x (String.length line)) in
              let after_start = start_x + form_width in
              let after =
                if after_start < String.length line then
                  String.sub line after_start (String.length line - after_start)
                else ""
              in
              before ^ form_line ^ after
            else line)
          !lines
  | None -> ());

  (* Render confirm modal if active *)
  (match app.App.confirm_modal with
  | Some modal ->
      let modal_width = min 50 (width - 10) in
      let modal_lines = Modal.render_confirm modal modal_width in
      let modal_height = List.length modal_lines in
      let start_y = (height - modal_height) / 2 in
      let start_x = (width - modal_width) / 2 in
      lines :=
        List.mapi
          (fun i line ->
            if i >= start_y && i < start_y + modal_height then
              let modal_line = List.nth modal_lines (i - start_y) in
              let before = String.sub line 0 (min start_x (String.length line)) in
              let after_start = start_x + modal_width in
              let after =
                if after_start < String.length line then
                  String.sub line after_start (String.length line - after_start)
                else ""
              in
              before ^ modal_line ^ after
            else line)
          !lines
  | None -> ());

  (* Render help overlay if active *)
  if app.App.show_help then (
    let help_width = min 70 (width - 10) in
    let help_height = min 40 (height - 4) in
    let help_lines = Help.render help_width help_height in
    let start_y = (height - help_height) / 2 in
    let start_x = (width - help_width) / 2 in
    lines :=
      List.mapi
        (fun i line ->
          if i >= start_y && i < start_y + List.length help_lines then
            let help_line = List.nth help_lines (i - start_y) in
            let before = String.sub line 0 (min start_x (String.length line)) in
            let after_start = start_x + help_width in
            let after =
              if after_start < String.length line then
                String.sub line after_start (String.length line - after_start)
              else ""
            in
            before ^ help_line ^ after
          else line)
        !lines);

  (* Render notification if active *)
  (match app.App.notification with
  | Some notif ->
      let notif_lines = Modal.render_notification notif width in
      lines := !lines @ notif_lines
  | None -> ());

  (* Render status bar *)
  lines := !lines @ [render_status_bar app width];

  (* Pad to height *)
  let current_height = List.length !lines in
  if current_height < height then
    for _ = current_height to height - 1 do
      lines := !lines @ [String.make width ' ']
    done;

  (* Join all lines *)
  String.concat "\n" !lines
