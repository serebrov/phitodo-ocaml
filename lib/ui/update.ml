(** Event handling / Update function for TEA pattern *)

(** Message type *)
type msg =
  | Tick
  | Quit

(** Handle key in normal mode *)
let handle_normal_key app key =
  match key with
  (* Quit *)
  | Minttea.Event.Key "q" ->
      (app, Some Quit)

  (* Help *)
  | Minttea.Event.Key "?" ->
      (App.toggle_help app, None)

  (* View shortcuts *)
  | Minttea.Event.Key "1" -> (App.switch_to_view app Sidebar.Inbox, None)
  | Minttea.Event.Key "2" -> (App.switch_to_view app Sidebar.Today, None)
  | Minttea.Event.Key "3" -> (App.switch_to_view app Sidebar.Upcoming, None)
  | Minttea.Event.Key "4" -> (App.switch_to_view app Sidebar.Anytime, None)
  | Minttea.Event.Key "5" -> (App.switch_to_view app Sidebar.Completed, None)
  | Minttea.Event.Key "6" -> (App.switch_to_view app Sidebar.Review, None)
  | Minttea.Event.Key "7" when Config.has_github app.App.config ->
      (App.switch_to_view app Sidebar.GitHub, None)
  | Minttea.Event.Key "8" when Config.has_toggl app.App.config ->
      (App.switch_to_view app Sidebar.Toggl, None)
  | Minttea.Event.Key "9" -> (App.switch_to_view app Sidebar.Settings, None)

  (* Focus navigation *)
  | Minttea.Event.Key "\t" -> (App.cycle_focus app, None)  (* Tab *)
  | Minttea.Event.Key "h" | Minttea.Event.Left -> (App.cycle_focus_reverse app, None)
  | Minttea.Event.Key "l" | Minttea.Event.Right -> (App.cycle_focus app, None)

  (* List navigation *)
  | Minttea.Event.Key "j" | Minttea.Event.Down ->
      let app =
        match app.App.focus with
        | App.Sidebar ->
            { app with sidebar = Sidebar.select_next app.sidebar }
        | App.List | App.Detail -> (
            match app.current_view with
            | App.Inbox -> { app with inbox_view = Inbox_view.select_next app.inbox_view }
            | App.Today -> { app with today_view = Today_view.select_next app.today_view }
            | App.Upcoming -> { app with upcoming_view = Upcoming_view.select_next app.upcoming_view }
            | App.Anytime -> { app with anytime_view = Anytime_view.select_next app.anytime_view }
            | App.Completed -> { app with completed_view = Completed_view.select_next app.completed_view }
            | App.Review -> { app with review_view = Review_view.select_next app.review_view }
            | App.ProjectView _ -> { app with project_view = Project_view.select_next app.project_view }
            | App.TagView _ -> { app with tag_view = Tag_view.select_next app.tag_view }
            | App.GitHub -> { app with github_view = Github_view.select_next app.github_view }
            | App.Toggl -> { app with toggl_view = Toggl_view.select_next app.toggl_view }
            | App.SettingsView -> { app with settings_view = Settings_view.select_next app.settings_view })
      in
      (app, None)
  | Minttea.Event.Key "k" | Minttea.Event.Up ->
      let app =
        match app.App.focus with
        | App.Sidebar ->
            { app with sidebar = Sidebar.select_previous app.sidebar }
        | App.List | App.Detail -> (
            match app.current_view with
            | App.Inbox -> { app with inbox_view = Inbox_view.select_previous app.inbox_view }
            | App.Today -> { app with today_view = Today_view.select_previous app.today_view }
            | App.Upcoming -> { app with upcoming_view = Upcoming_view.select_previous app.upcoming_view }
            | App.Anytime -> { app with anytime_view = Anytime_view.select_previous app.anytime_view }
            | App.Completed -> { app with completed_view = Completed_view.select_previous app.completed_view }
            | App.Review -> { app with review_view = Review_view.select_previous app.review_view }
            | App.ProjectView _ -> { app with project_view = Project_view.select_previous app.project_view }
            | App.TagView _ -> { app with tag_view = Tag_view.select_previous app.tag_view }
            | App.GitHub -> { app with github_view = Github_view.select_previous app.github_view }
            | App.Toggl -> { app with toggl_view = Toggl_view.select_previous app.toggl_view }
            | App.SettingsView -> { app with settings_view = Settings_view.select_previous app.settings_view })
      in
      (app, None)
  | Minttea.Event.Key "g" ->
      let app =
        match app.App.focus with
        | App.Sidebar ->
            { app with sidebar = Sidebar.select_first app.sidebar }
        | App.List | App.Detail -> (
            match app.current_view with
            | App.Inbox -> { app with inbox_view = Inbox_view.select_first app.inbox_view }
            | App.Today -> { app with today_view = Today_view.select_first app.today_view }
            | App.Upcoming -> { app with upcoming_view = Upcoming_view.select_first app.upcoming_view }
            | App.Anytime -> { app with anytime_view = Anytime_view.select_first app.anytime_view }
            | App.Completed -> { app with completed_view = Completed_view.select_first app.completed_view }
            | App.Review -> { app with review_view = Review_view.select_first app.review_view }
            | App.ProjectView _ -> { app with project_view = Project_view.select_first app.project_view }
            | App.TagView _ -> { app with tag_view = Tag_view.select_first app.tag_view }
            | App.GitHub -> { app with github_view = Github_view.select_first app.github_view }
            | App.Toggl -> { app with toggl_view = Toggl_view.select_first app.toggl_view }
            | _ -> app)
      in
      (app, None)
  | Minttea.Event.Key "G" ->
      let app =
        match app.App.focus with
        | App.Sidebar ->
            { app with sidebar = Sidebar.select_last app.sidebar }
        | App.List | App.Detail -> (
            match app.current_view with
            | App.Inbox -> { app with inbox_view = Inbox_view.select_last app.inbox_view }
            | App.Today -> { app with today_view = Today_view.select_last app.today_view }
            | App.Upcoming -> { app with upcoming_view = Upcoming_view.select_last app.upcoming_view }
            | App.Anytime -> { app with anytime_view = Anytime_view.select_last app.anytime_view }
            | App.Completed -> { app with completed_view = Completed_view.select_last app.completed_view }
            | App.Review -> { app with review_view = Review_view.select_last app.review_view }
            | App.ProjectView _ -> { app with project_view = Project_view.select_last app.project_view }
            | App.TagView _ -> { app with tag_view = Tag_view.select_last app.tag_view }
            | App.GitHub -> { app with github_view = Github_view.select_last app.github_view }
            | App.Toggl -> { app with toggl_view = Toggl_view.select_last app.toggl_view }
            | _ -> app)
      in
      (app, None)

  (* Enter on sidebar selects view *)
  | Minttea.Event.Enter when app.App.focus = App.Sidebar -> (
      match Sidebar.selected_item app.sidebar with
      | Some item -> (App.switch_to_view app item, None)
      | None -> (app, None))

  (* Task actions *)
  | Minttea.Event.Space -> (App.toggle_task_completed app, None)
  | Minttea.Event.Key "n" -> (App.start_new_task app, None)
  | Minttea.Event.Key "e" -> (App.start_edit_task app, None)
  | Minttea.Event.Key "d" -> (App.start_delete app, None)
  | Minttea.Event.Key "o" -> (App.open_task_url app, None)
  | Minttea.Event.Key "r" -> (App.refresh app, None)

  (* Status shortcuts *)
  | Minttea.Event.Key "i" -> (App.set_task_status app Task.Inbox, None)
  | Minttea.Event.Key "a" -> (App.set_task_status app Task.Active, None)
  | Minttea.Event.Key "s" -> (App.set_task_status app Task.Scheduled, None)

  (* GitHub column navigation *)
  | Minttea.Event.Key "H" when app.current_view = App.GitHub ->
      ({ app with github_view = Github_view.prev_column app.github_view }, None)
  | Minttea.Event.Key "L" when app.current_view = App.GitHub ->
      ({ app with github_view = Github_view.next_column app.github_view }, None)

  | _ -> (app, None)

(** Handle key in task form mode *)
let handle_task_form_key app key =
  match app.App.task_form with
  | None -> (app, None)
  | Some form ->
      match key with
      | Minttea.Event.Escape -> (App.cancel_task_form app, None)
      | Minttea.Event.Enter ->
          let current = form.Task_form.current_field in
          (match current with
          | Task_form.Title | Task_form.Notes | Task_form.DueDate ->
              (App.save_task_form app, None)
          | _ ->
              let form =
                match current with
                | Task_form.Project -> Task_form.cycle_project form
                | Task_form.Priority -> Task_form.cycle_priority form
                | Task_form.Status -> Task_form.cycle_status form
                | Task_form.Kind -> Task_form.cycle_kind form
                | Task_form.Size -> Task_form.cycle_size form
                | _ -> form
              in
              ({ app with task_form = Some form }, None))
      | Minttea.Event.Key "\t" ->  (* Tab *)
          ({ app with task_form = Some (Task_form.tab_next form) }, None)
      | Minttea.Event.Backspace -> (
          match form.current_field with
          | Task_form.Title ->
              let title = form.title_input in
              if String.length title > 0 then
                let new_title = String.sub title 0 (String.length title - 1) in
                ({ app with task_form = Some (Task_form.set_title form new_title) }, None)
              else (app, None)
          | Task_form.Notes ->
              let notes = form.notes_input in
              if String.length notes > 0 then
                let new_notes = String.sub notes 0 (String.length notes - 1) in
                ({ app with task_form = Some (Task_form.set_notes form new_notes) }, None)
              else (app, None)
          | Task_form.DueDate ->
              let due = form.due_date_input in
              if String.length due > 0 then
                let new_due = String.sub due 0 (String.length due - 1) in
                ({ app with task_form = Some (Task_form.set_due_date form new_due) }, None)
              else (app, None)
          | _ -> (app, None))
      | Minttea.Event.Space -> (
          match form.current_field with
          | Task_form.Title ->
              let new_title = form.title_input ^ " " in
              ({ app with task_form = Some (Task_form.set_title form new_title) }, None)
          | Task_form.Notes ->
              let new_notes = form.notes_input ^ " " in
              ({ app with task_form = Some (Task_form.set_notes form new_notes) }, None)
          | _ -> (app, None))
      | Minttea.Event.Key c when String.length c = 1 -> (
          match form.current_field with
          | Task_form.Title ->
              let new_title = form.title_input ^ c in
              ({ app with task_form = Some (Task_form.set_title form new_title) }, None)
          | Task_form.Notes ->
              let new_notes = form.notes_input ^ c in
              ({ app with task_form = Some (Task_form.set_notes form new_notes) }, None)
          | Task_form.DueDate ->
              let new_due = form.due_date_input ^ c in
              ({ app with task_form = Some (Task_form.set_due_date form new_due) }, None)
          | _ -> (app, None))
      | _ -> (app, None)

(** Handle key in confirm mode *)
let handle_confirm_key app key =
  match app.App.confirm_modal with
  | None -> (app, None)
  | Some modal ->
      match key with
      | Minttea.Event.Escape -> (App.cancel_confirm app, None)
      | Minttea.Event.Enter -> (App.execute_confirm app, None)
      | Minttea.Event.Left | Minttea.Event.Key "h" | Minttea.Event.Key "n" ->
          ({ app with confirm_modal = Some (Modal.select_cancel modal) }, None)
      | Minttea.Event.Right | Minttea.Event.Key "l" | Minttea.Event.Key "y" ->
          ({ app with confirm_modal = Some (Modal.select_confirm modal) }, None)
      | Minttea.Event.Key "\t" ->  (* Tab *)
          ({ app with confirm_modal = Some (Modal.toggle_confirm modal) }, None)
      | _ -> (app, None)

(** Handle key in settings mode *)
let handle_settings_key app key =
  let view = app.App.settings_view in
  match key with
  | Minttea.Event.Escape ->
      if view.Settings_view.editing then
        ({ app with settings_view = Settings_view.cancel_edit view }, None)
      else
        let app = { app with current_view = App.Inbox } in
        (App.switch_to_view app Sidebar.Inbox, None)
  | Minttea.Event.Enter ->
      if view.editing then (
        let view = Settings_view.confirm_edit view in
        let app = { app with settings_view = view; config = view.config } in
        (app, None))
      else
        ({ app with settings_view = Settings_view.start_edit view }, None)
  | Minttea.Event.Key "j" | Minttea.Event.Down when not view.editing ->
      ({ app with settings_view = Settings_view.select_next view }, None)
  | Minttea.Event.Key "k" | Minttea.Event.Up when not view.editing ->
      ({ app with settings_view = Settings_view.select_previous view }, None)
  | Minttea.Event.Backspace when view.editing ->
      ({ app with settings_view = Settings_view.input_backspace view }, None)
  | Minttea.Event.Space when view.editing ->
      ({ app with settings_view = Settings_view.input_char view ' ' }, None)
  | Minttea.Event.Key c when view.editing && String.length c = 1 ->
      ({ app with settings_view = Settings_view.input_char view c.[0] }, None)
  | _ -> (app, None)

(** Main update function *)
let update (app : App.t) (key : Minttea.Event.key) : App.t * msg option =
  (* Clear notification on any key *)
  let app = App.clear_notification app in

  (* Process any pending async messages *)
  let app = App.process_async_messages app in

  (* Handle help overlay *)
  if app.show_help then
    match key with
    | Minttea.Event.Escape | Minttea.Event.Key "?" -> (App.toggle_help app, None)
    | _ -> (app, None)
  else
    (* Handle based on mode *)
    match app.mode with
    | App.Normal -> handle_normal_key app key
    | App.TaskForm -> handle_task_form_key app key
    | App.Confirm -> handle_confirm_key app key
    | App.Settings -> handle_settings_key app key
    | App.Input -> (app, None)
