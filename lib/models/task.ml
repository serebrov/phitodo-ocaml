(** Task data model with enums for status, priority, kind, and size *)

(** Task status representing the lifecycle of a task *)
type task_status =
  | Inbox
  | Active
  | Scheduled
  | Completed
  | Cancelled
[@@deriving yojson]

let task_status_to_string = function
  | Inbox -> "inbox"
  | Active -> "active"
  | Scheduled -> "scheduled"
  | Completed -> "completed"
  | Cancelled -> "cancelled"

let task_status_of_string = function
  | "inbox" -> Inbox
  | "active" -> Active
  | "scheduled" -> Scheduled
  | "completed" -> Completed
  | "cancelled" -> Cancelled
  | s -> failwith ("Unknown task status: " ^ s)

let task_status_display = function
  | Inbox -> "Inbox"
  | Active -> "Active"
  | Scheduled -> "Scheduled"
  | Completed -> "Completed"
  | Cancelled -> "Cancelled"

(** Task priority levels *)
type task_priority =
  | None
  | Low
  | Medium
  | High
[@@deriving yojson]

let task_priority_to_string = function
  | None -> "none"
  | Low -> "low"
  | Medium -> "medium"
  | High -> "high"

let task_priority_of_string = function
  | "none" -> None
  | "low" -> Low
  | "medium" -> Medium
  | "high" -> High
  | s -> failwith ("Unknown task priority: " ^ s)

let task_priority_symbol = function
  | None -> " "
  | Low -> "!"
  | Medium -> "!!"
  | High -> "!!!"

let task_priority_display = function
  | None -> "None"
  | Low -> "Low"
  | Medium -> "Medium"
  | High -> "High"

(** Task kind/type *)
type task_kind =
  | Task
  | Bug
  | Feature
  | Chore
  | GhIssue
  | GhPr
  | GhReview
[@@deriving yojson]

let task_kind_to_string = function
  | Task -> "task"
  | Bug -> "bug"
  | Feature -> "feature"
  | Chore -> "chore"
  | GhIssue -> "gh_issue"
  | GhPr -> "gh_pr"
  | GhReview -> "gh_review"

let task_kind_of_string = function
  | "task" -> Task
  | "bug" -> Bug
  | "feature" -> Feature
  | "chore" -> Chore
  | "gh_issue" | "gh:issue" -> GhIssue
  | "gh_pr" | "gh:pr" -> GhPr
  | "gh_review" | "gh:review" -> GhReview
  | s -> failwith ("Unknown task kind: " ^ s)

let task_kind_symbol = function
  | Task -> "[T]"
  | Bug -> "[B]"
  | Feature -> "[F]"
  | Chore -> "[C]"
  | GhIssue -> "[ISS]"
  | GhPr -> "[PR]"
  | GhReview -> "[REV]"

let task_kind_display = function
  | Task -> "Task"
  | Bug -> "Bug"
  | Feature -> "Feature"
  | Chore -> "Chore"
  | GhIssue -> "Issue"
  | GhPr -> "PR"
  | GhReview -> "Review"

(** Task size estimate *)
type task_size =
  | Xs
  | S
  | M
  | L
[@@deriving yojson]

let task_size_to_string = function
  | Xs -> "xs"
  | S -> "s"
  | M -> "m"
  | L -> "l"

let task_size_of_string = function
  | "xs" -> Xs
  | "s" -> S
  | "m" -> M
  | "l" -> L
  | s -> failwith ("Unknown task size: " ^ s)

let task_size_display = function
  | Xs -> "XS"
  | S -> "S"
  | M -> "M"
  | L -> "L"

(** Main task record *)
type t = {
  id : string;
  title : string;
  notes : string option;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  due_date : Ptime.date option;
  start_date : Ptime.date option;
  completed_at : Ptime.t option;
  project_id : string option;
  priority : task_priority;
  tags : string list;
  status : task_status;
  order_index : int64;
  deleted : bool;
  kind : task_kind option;
  size : task_size option;
  assignee : string option;
  context_url : string option;
  metadata : (string * string) list;
}

(** Create a new task with defaults *)
let create ~title ?(notes = Option.None) ?(due_date = Option.None)
    ?(start_date = Option.None) ?(project_id = Option.None)
    ?(priority = None) ?(tags = []) ?(status = Inbox) ?(order_index = 0L)
    ?(kind = Option.None) ?(size = Option.None) ?(assignee = Option.None)
    ?(context_url = Option.None) ?(metadata = []) () =
  let now = Ptime_clock.now () in
  let id = Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string in
  {
    id;
    title;
    notes;
    created_at = now;
    updated_at = now;
    due_date;
    start_date;
    completed_at = Option.None;
    project_id;
    priority;
    tags;
    status;
    order_index;
    deleted = false;
    kind;
    size;
    assignee;
    context_url;
    metadata;
  }

(** Mark a task as completed *)
let complete task =
  let now = Ptime_clock.now () in
  { task with status = Completed; completed_at = Some now; updated_at = now }

(** Mark a task as deleted (soft delete) *)
let delete task =
  let now = Ptime_clock.now () in
  { task with deleted = true; updated_at = now }

(** Update task's updated_at timestamp *)
let touch task =
  let now = Ptime_clock.now () in
  { task with updated_at = now }

(** Check if task is overdue *)
let is_overdue task =
  match task.due_date with
  | Option.None -> false
  | Some (y, m, d) -> (
      let today = Ptime_clock.now () |> Ptime.to_date in
      let ty, tm, td = today in
      match Ptime.of_date (y, m, d) with
      | Option.None -> false
      | Some due_ptime -> (
          match Ptime.of_date (ty, tm, td) with
          | Option.None -> false
          | Some today_ptime ->
              task.status <> Completed
              && Ptime.is_earlier due_ptime ~than:today_ptime))

(** Check if task is due today *)
let is_due_today task =
  match task.due_date with
  | Option.None -> false
  | Some due ->
      let today = Ptime_clock.now () |> Ptime.to_date in
      due = today

(** Format due date for display *)
let format_due_date task =
  match task.due_date with
  | Option.None -> ""
  | Some (y, m, d) -> Printf.sprintf "%04d-%02d-%02d" y m d

(** Parse a date string (YYYY-MM-DD) to Ptime.date option *)
let parse_date_string s =
  match String.split_on_char '-' s with
  | [ y; m; d ] -> (
      try
        let year = int_of_string y in
        let month = int_of_string m in
        let day = int_of_string d in
        Some (year, month, day)
      with _ -> Option.None)
  | _ -> Option.None

(** Format Ptime.t to RFC3339 string *)
let ptime_to_rfc3339 t =
  let (y, m, d), ((hh, mm, ss), _tz_offset) = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" y m d hh mm ss

(** Parse RFC3339 string to Ptime.t option *)
let ptime_of_rfc3339 s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> Some t
  | Error _ -> Option.None
