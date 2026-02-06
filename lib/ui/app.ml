(** Main application state and TEA model *)

(** Application mode *)
type app_mode =
  | Normal
  | Input
  | TaskForm
  | Confirm
  | Settings

(** Focus area *)
type focus_area =
  | Sidebar
  | List
  | Detail

(** Current view *)
type current_view =
  | Inbox
  | Today
  | Upcoming
  | Anytime
  | Completed
  | Review
  | GitHub
  | Toggl
  | SettingsView
  | ProjectView of string
  | TagView of string

(** Async message for background operations *)
type async_message =
  | GitHubDataReady of (Github_service.github_data, string) result
  | TogglDataReady of (Toggl_service.toggl_data, string) result

(** Application state *)
type t = {
  config : Config.t;
  db : Sqlite3.db;
  mode : app_mode;
  focus : focus_area;
  current_view : current_view;
  show_help : bool;

  (* Data *)
  tasks : Task.t list;
  projects : Project.t list;
  tags : Tag.t list;

  (* UI State *)
  sidebar : Sidebar.t;
  inbox_view : Inbox_view.t;
  today_view : Today_view.t;
  upcoming_view : Upcoming_view.t;
  anytime_view : Anytime_view.t;
  completed_view : Completed_view.t;
  review_view : Review_view.t;
  project_view : Project_view.t;
  tag_view : Tag_view.t;
  github_view : Github_view.t;
  toggl_view : Toggl_view.t;
  settings_view : Settings_view.t;

  (* Modals *)
  task_form : Task_form.t option;
  confirm_modal : Modal.confirm_t option;
  notification : Modal.notification_t option;
  pending_delete_id : string option;

  (* Input *)
  search_input : Input.t;
  search_query : string;

  (* Async state *)
  async_messages : async_message list;
}

(** Create new application state *)
let create config =
  let db_path = Config.ensure_database () in
  let db = Repository.open_db db_path in
  {
    config;
    db;
    mode = Normal;
    focus = Sidebar;
    current_view = Inbox;
    show_help = false;
    tasks = [];
    projects = [];
    tags = [];
    sidebar = Sidebar.create
      ~show_github:(Config.has_github config)
      ~show_toggl:(Config.has_toggl config) ();
    inbox_view = Inbox_view.create ();
    today_view = Today_view.create ();
    upcoming_view = Upcoming_view.create ();
    anytime_view = Anytime_view.create ();
    completed_view = Completed_view.create ();
    review_view = Review_view.create ();
    project_view = Project_view.create ~project_id:"" ~project_name:"" ();
    tag_view = Tag_view.create ~tag_id:"" ~tag_name:"" ();
    github_view = Github_view.create ();
    toggl_view = Toggl_view.create ();
    settings_view = Settings_view.create config;
    task_form = None;
    confirm_modal = None;
    notification = None;
    pending_delete_id = None;
    search_input = Input.create ~placeholder:"Search..." ();
    search_query = "";
    async_messages = [];
  }

(** Load all data from database *)
let load_data app =
  let tasks = Repository.Task.get_all app.db in
  let projects = Repository.Project.get_all app.db in
  let tags = Repository.Tag.get_all app.db in
  { app with tasks; projects; tags }

(** Update sidebar counts *)
let update_sidebar_counts app =
  let counts : Sidebar.sidebar_counts =
    {
      inbox = Filter_service.count_inbox app.tasks;
      today = Filter_service.count_today app.tasks;
      upcoming = Filter_service.count_upcoming app.tasks;
      anytime = Filter_service.count_anytime app.tasks;
      completed = Filter_service.count_completed app.tasks;
      review = Filter_service.count_review app.tasks;
    }
  in
  let project_counts =
    List.map
      (fun (p : Project.t) ->
        (p.id, Filter_service.count_by_project app.tasks p.id))
      app.projects
  in
  let tag_counts =
    List.map
      (fun (t : Tag.t) -> (t.id, Filter_service.count_by_tag app.tasks t.id))
      app.tags
  in
  let sidebar = Sidebar.update_counts app.sidebar counts in
  let sidebar = Sidebar.set_projects sidebar app.projects project_counts in
  let sidebar = Sidebar.set_tags sidebar app.tags tag_counts in
  { app with sidebar }

(** Update all views with current tasks *)
let update_views app =
  let inbox_view = Inbox_view.set_tasks app.inbox_view (Inbox_view.filter_tasks app.tasks) in
  let today_view = Today_view.set_tasks app.today_view (Today_view.filter_tasks app.tasks) in
  let upcoming_view = Upcoming_view.set_tasks app.upcoming_view (Upcoming_view.filter_tasks app.tasks) in
  let anytime_view = Anytime_view.set_tasks app.anytime_view (Anytime_view.filter_tasks app.tasks) in
  let completed_view = Completed_view.set_tasks app.completed_view (Completed_view.filter_tasks app.tasks) in
  let review_view = Review_view.set_tasks app.review_view (Review_view.filter_tasks app.tasks) in
  let project_view = Project_view.set_tasks app.project_view app.tasks in
  let tag_view = Tag_view.set_tasks app.tag_view app.tasks in
  { app with
    inbox_view;
    today_view;
    upcoming_view;
    anytime_view;
    completed_view;
    review_view;
    project_view;
    tag_view;
  }

(** Full data refresh *)
let refresh app =
  let app = load_data app in
  let app = update_sidebar_counts app in
  update_views app

(** Switch to a view based on sidebar item *)
let switch_to_view app item =
  let current_view =
    match item with
    | Sidebar.Inbox -> Inbox
    | Sidebar.Today -> Today
    | Sidebar.Upcoming -> Upcoming
    | Sidebar.Anytime -> Anytime
    | Sidebar.Completed -> Completed
    | Sidebar.Review -> Review
    | Sidebar.GitHub -> GitHub
    | Sidebar.Toggl -> Toggl
    | Sidebar.Settings -> SettingsView
    | Sidebar.ProjectItem pid -> ProjectView pid
    | Sidebar.TagItem tid -> TagView tid
  in
  let app = { app with current_view; focus = List } in
  (* Update project/tag view if needed *)
  match current_view with
  | ProjectView pid ->
      let project = List.find_opt (fun (p : Project.t) -> p.id = pid) app.projects in
      let name = match project with Some p -> p.name | None -> "Project" in
      let project_view = Project_view.set_project app.project_view ~project_id:pid ~project_name:name in
      let project_view = Project_view.set_tasks project_view app.tasks in
      { app with project_view }
  | TagView tid ->
      let tag = List.find_opt (fun (t : Tag.t) -> t.id = tid) app.tags in
      let name = match tag with Some t -> t.name | None -> "Tag" in
      let tag_view = Tag_view.set_tag app.tag_view ~tag_id:tid ~tag_name:name in
      let tag_view = Tag_view.set_tasks tag_view app.tasks in
      { app with tag_view }
  | _ -> app

(** Cycle focus between sidebar, list, and detail *)
let cycle_focus app =
  let new_focus =
    match app.focus with
    | Sidebar -> List
    | List -> Detail
    | Detail -> Sidebar
  in
  { app with focus = new_focus }

(** Reverse cycle focus *)
let cycle_focus_reverse app =
  let new_focus =
    match app.focus with
    | Sidebar -> Detail
    | List -> Sidebar
    | Detail -> List
  in
  { app with focus = new_focus }

(** Get currently selected task based on current view *)
let get_selected_task app =
  match app.current_view with
  | Inbox -> Inbox_view.selected_task app.inbox_view
  | Today -> Today_view.selected_task app.today_view
  | Upcoming -> Upcoming_view.selected_task app.upcoming_view
  | Anytime -> Anytime_view.selected_task app.anytime_view
  | Completed -> Completed_view.selected_task app.completed_view
  | Review -> Review_view.selected_task app.review_view
  | ProjectView _ -> Project_view.selected_task app.project_view
  | TagView _ -> Tag_view.selected_task app.tag_view
  | _ -> None

(** Toggle task completion *)
let toggle_task_completed app =
  match get_selected_task app with
  | None -> app
  | Some task ->
      let updated =
        if task.status = Task.Completed then
          { task with status = Task.Active; completed_at = None; updated_at = Ptime_clock.now () }
        else Task.complete task
      in
      Repository.Task.update app.db updated;
      refresh app

(** Set task priority *)
let set_task_priority app priority =
  match get_selected_task app with
  | None -> app
  | Some task ->
      let updated = { task with priority; updated_at = Ptime_clock.now () } in
      Repository.Task.update app.db updated;
      refresh app

(** Set task status *)
let set_task_status app status =
  match get_selected_task app with
  | None -> app
  | Some task ->
      let updated = { task with status; updated_at = Ptime_clock.now () } in
      Repository.Task.update app.db updated;
      refresh app

(** Start creating a new task *)
let start_new_task app =
  let project_id =
    match app.current_view with
    | ProjectView pid -> Some pid
    | _ -> None
  in
  let form = Task_form.create_new ~project_id app.projects in
  { app with task_form = Some form; mode = TaskForm }

(** Start editing a task *)
let start_edit_task app =
  match get_selected_task app with
  | None -> app
  | Some task ->
      let form = Task_form.create_edit task app.projects in
      { app with task_form = Some form; mode = TaskForm }

(** Save task from form *)
let save_task_form app =
  match app.task_form with
  | None -> app
  | Some form ->
      if Task_form.is_valid form then (
        let task = Task_form.build_task form in
        if form.is_new then Repository.Task.insert app.db task
        else Repository.Task.update app.db task;
        let app = { app with task_form = None; mode = Normal } in
        refresh app)
      else app

(** Cancel task form *)
let cancel_task_form app =
  { app with task_form = None; mode = Normal }

(** Start delete confirmation *)
let start_delete app =
  match get_selected_task app with
  | None -> app
  | Some task ->
      let modal =
        Modal.create_confirm ~title:"Delete Task"
          ~message:(Printf.sprintf "Delete \"%s\"?" task.title)
          ~confirm_label:"Delete" ~cancel_label:"Cancel" ()
      in
      { app with
        confirm_modal = Some modal;
        pending_delete_id = Some task.id;
        mode = Confirm;
      }

(** Execute confirmation action *)
let execute_confirm app =
  match app.confirm_modal with
  | None -> { app with mode = Normal }
  | Some modal ->
      if modal.selected_confirm then (
        match app.pending_delete_id with
        | Some id ->
            Repository.Task.delete app.db id;
            let app = { app with confirm_modal = None; pending_delete_id = None; mode = Normal } in
            refresh app
        | None -> { app with confirm_modal = None; mode = Normal })
      else { app with confirm_modal = None; pending_delete_id = None; mode = Normal }

(** Cancel confirmation *)
let cancel_confirm app =
  { app with confirm_modal = None; pending_delete_id = None; mode = Normal }

(** Open task URL *)
let open_task_url app =
  match get_selected_task app with
  | None -> app
  | Some task -> (
      match task.context_url with
      | Some url ->
          (* Use system open command *)
          let cmd =
            if Sys.os_type = "Unix" then
              if Sys.file_exists "/usr/bin/open" then "open"
              else "xdg-open"
            else "start"
          in
          ignore (Sys.command (Printf.sprintf "%s '%s'" cmd url));
          app
      | None -> app)

(** Show notification *)
let notify app ?(is_error = false) message =
  { app with notification = Some (Modal.create_notification ~is_error message) }

(** Clear notification *)
let clear_notification app = { app with notification = None }

(** Toggle help overlay *)
let toggle_help app = { app with show_help = not app.show_help }

(** Add async message *)
let add_async_message app msg =
  { app with async_messages = msg :: app.async_messages }

(** Process async messages *)
let process_async_messages app =
  let rec process app = function
    | [] -> { app with async_messages = [] }
    | msg :: rest ->
        let app =
          match msg with
          | GitHubDataReady (Ok data) ->
              let github_view =
                Github_view.set_data app.github_view
                  ~review_prs:data.review_prs
                  ~my_prs:data.my_prs
                  ~assigned_issues:data.assigned_issues
              in
              (* Sync to tasks *)
              Github_service.sync_to_tasks ~db:app.db data;
              let app = { app with github_view } in
              refresh app
          | GitHubDataReady (Error e) ->
              let github_view = Github_view.set_error app.github_view e in
              { app with github_view }
          | TogglDataReady (Ok data) ->
              let toggl_view = Toggl_view.set_data app.toggl_view data.entries data.projects in
              { app with toggl_view }
          | TogglDataReady (Error e) ->
              let toggl_view = Toggl_view.set_error app.toggl_view e in
              { app with toggl_view }
        in
        process app rest
  in
  process app app.async_messages

(** Close the application *)
let close app =
  (* Save config *)
  Config.save app.config;
  (* Close database *)
  Repository.close_db app.db
