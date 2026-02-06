(** Task form component for creating and editing tasks *)

(** Form field types *)
type form_field =
  | Title
  | Notes
  | DueDate
  | Project
  | Priority
  | Status
  | Kind
  | Size

(** Task form state *)
type t = {
  task : Task.t;
  is_new : bool;
  current_field : form_field;
  title_input : string;
  notes_input : string;
  due_date_input : string;
  available_projects : Project.t list;
  selected_project_index : int option;
}

(** Create a new task form for creating a task *)
let create_new ~project_id projects =
  let task = Task.create ~title:"" () in
  let task = { task with project_id } in
  let selected_project_index =
    match project_id with
    | Some pid ->
        List.find_mapi
          (fun i (p : Project.t) -> if p.id = pid then Some i else None)
          projects
    | None -> None
  in
  {
    task;
    is_new = true;
    current_field = Title;
    title_input = "";
    notes_input = "";
    due_date_input = "";
    available_projects = projects;
    selected_project_index;
  }

(** Create a task form for editing an existing task *)
let create_edit task projects =
  let due_date_input =
    match task.Task.due_date with
    | Some (y, m, d) -> Printf.sprintf "%04d-%02d-%02d" y m d
    | None -> ""
  in
  let notes_input = Option.value ~default:"" task.notes in
  let selected_project_index =
    match task.project_id with
    | Some pid ->
        List.find_mapi
          (fun i (p : Project.t) -> if p.id = pid then Some i else None)
          projects
    | None -> None
  in
  {
    task;
    is_new = false;
    current_field = Title;
    title_input = task.title;
    notes_input;
    due_date_input;
    available_projects = projects;
    selected_project_index;
  }

(** Get the next field in tab order *)
let next_field = function
  | Title -> Notes
  | Notes -> DueDate
  | DueDate -> Project
  | Project -> Priority
  | Priority -> Status
  | Status -> Kind
  | Kind -> Size
  | Size -> Title

(** Get the previous field in tab order *)
let prev_field = function
  | Title -> Size
  | Notes -> Title
  | DueDate -> Notes
  | Project -> DueDate
  | Priority -> Project
  | Status -> Priority
  | Kind -> Status
  | Size -> Kind

(** Move to next field *)
let tab_next form = { form with current_field = next_field form.current_field }

(** Move to previous field *)
let tab_prev form = { form with current_field = prev_field form.current_field }

(** Update title input *)
let set_title form title = { form with title_input = title }

(** Update notes input *)
let set_notes form notes = { form with notes_input = notes }

(** Update due date input *)
let set_due_date form due_date = { form with due_date_input = due_date }

(** Cycle through projects *)
let cycle_project form =
  let num_projects = List.length form.available_projects in
  if num_projects = 0 then form
  else
    let new_index =
      match form.selected_project_index with
      | None -> Some 0
      | Some i -> if i + 1 >= num_projects then None else Some (i + 1)
    in
    { form with selected_project_index = new_index }

(** Cycle through priorities *)
let cycle_priority form =
  let new_priority =
    match form.task.Task.priority with
    | Task.None -> Task.Low
    | Task.Low -> Task.Medium
    | Task.Medium -> Task.High
    | Task.High -> Task.None
  in
  { form with task = { form.task with priority = new_priority } }

(** Cycle through statuses *)
let cycle_status form =
  let new_status =
    match form.task.Task.status with
    | Task.Inbox -> Task.Active
    | Task.Active -> Task.Scheduled
    | Task.Scheduled -> Task.Completed
    | Task.Completed -> Task.Cancelled
    | Task.Cancelled -> Task.Inbox
  in
  { form with task = { form.task with status = new_status } }

(** Cycle through kinds *)
let cycle_kind form =
  let new_kind =
    match form.task.Task.kind with
    | None -> Some Task.Task
    | Some Task.Task -> Some Task.Bug
    | Some Task.Bug -> Some Task.Feature
    | Some Task.Feature -> Some Task.Chore
    | Some Task.Chore -> Some Task.GhIssue
    | Some Task.GhIssue -> Some Task.GhPr
    | Some Task.GhPr -> Some Task.GhReview
    | Some Task.GhReview -> None
  in
  { form with task = { form.task with kind = new_kind } }

(** Cycle through sizes *)
let cycle_size form =
  let new_size =
    match form.task.Task.size with
    | None -> Some Task.Xs
    | Some Task.Xs -> Some Task.S
    | Some Task.S -> Some Task.M
    | Some Task.M -> Some Task.L
    | Some Task.L -> None
  in
  { form with task = { form.task with size = new_size } }

(** Build the final task from form state *)
let build_task form =
  let due_date =
    if String.length form.due_date_input > 0 then
      Task.parse_date_string form.due_date_input
    else None
  in
  let notes =
    if String.length form.notes_input > 0 then Some form.notes_input else None
  in
  let project_id =
    match form.selected_project_index with
    | Some i when i < List.length form.available_projects ->
        Some (List.nth form.available_projects i).id
    | _ -> None
  in
  { form.task with
    title = form.title_input;
    notes;
    due_date;
    project_id;
    updated_at = Ptime_clock.now ();
  }

(** Check if form is valid *)
let is_valid form = String.length form.title_input > 0

(** Field label *)
let field_label = function
  | Title -> "Title"
  | Notes -> "Notes"
  | DueDate -> "Due Date"
  | Project -> "Project"
  | Priority -> "Priority"
  | Status -> "Status"
  | Kind -> "Type"
  | Size -> "Size"

(** Render the form *)
let render form width =
  let lines = ref [] in

  (* Header *)
  let header =
    if form.is_new then "New Task" else "Edit Task"
  in
  lines := !lines @ [Theme.bold (Theme.colored Theme.Color.primary header)];
  lines := !lines @ [String.make width '-'];
  lines := !lines @ [""];

  let render_field field value is_current =
    let label = field_label field in
    let prefix = if is_current then "> " else "  " in
    let styled_value =
      if is_current then Theme.bold value else value
    in
    Printf.sprintf "%s%-12s: %s" prefix label styled_value
  in

  (* Title field *)
  lines := !lines @ [
    render_field Title form.title_input (form.current_field = Title)
  ];

  (* Notes field (truncated for display) *)
  let notes_display = Theme.truncate (width - 20) form.notes_input in
  lines := !lines @ [
    render_field Notes notes_display (form.current_field = Notes)
  ];

  (* Due date field *)
  let due_display =
    if String.length form.due_date_input = 0 then "(none)" else form.due_date_input
  in
  lines := !lines @ [
    render_field DueDate due_display (form.current_field = DueDate)
  ];

  (* Project field *)
  let project_display =
    match form.selected_project_index with
    | Some i when i < List.length form.available_projects ->
        (List.nth form.available_projects i).name
    | _ -> "(none)"
  in
  lines := !lines @ [
    render_field Project project_display (form.current_field = Project)
  ];

  (* Priority field *)
  let priority_display = Task.task_priority_display form.task.priority in
  lines := !lines @ [
    render_field Priority priority_display (form.current_field = Priority)
  ];

  (* Status field *)
  let status_display = Task.task_status_display form.task.status in
  lines := !lines @ [
    render_field Status status_display (form.current_field = Status)
  ];

  (* Kind field *)
  let kind_display =
    match form.task.kind with
    | Some k -> Task.task_kind_display k
    | None -> "(none)"
  in
  lines := !lines @ [
    render_field Kind kind_display (form.current_field = Kind)
  ];

  (* Size field *)
  let size_display =
    match form.task.size with
    | Some s -> Task.task_size_display s
    | None -> "(none)"
  in
  lines := !lines @ [
    render_field Size size_display (form.current_field = Size)
  ];

  lines := !lines @ [""];

  (* Help text *)
  lines := !lines @ [Theme.dim "Tab: Next field  Shift+Tab: Previous field"];
  lines := !lines @ [Theme.dim "Enter: Save  Esc: Cancel"];

  !lines
