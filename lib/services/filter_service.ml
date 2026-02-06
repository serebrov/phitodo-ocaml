(** Task filtering service *)

open Task

(** Get today's date as Ptime.date *)
let today () = Ptime.to_date (Ptime_clock.now ())

(** Compare two dates *)
let compare_date (y1, m1, d1) (y2, m2, d2) =
  let cmp_y = Int.compare y1 y2 in
  if cmp_y <> 0 then cmp_y
  else
    let cmp_m = Int.compare m1 m2 in
    if cmp_m <> 0 then cmp_m else Int.compare d1 d2

(** Check if date1 <= date2 *)
let date_le d1 d2 = compare_date d1 d2 <= 0

(** Check if date1 < date2 *)
let date_lt d1 d2 = compare_date d1 d2 < 0

(** Check if date1 > date2 *)
let date_gt d1 d2 = compare_date d1 d2 > 0

(** Filter tasks in inbox *)
let filter_inbox tasks =
  List.filter
    (fun task -> (not task.deleted) && task.status = Inbox)
    tasks

(** Filter tasks due today or overdue (but not completed) *)
let filter_today tasks =
  let today_date = today () in
  List.filter
    (fun task ->
      (not task.deleted)
      && task.status <> Completed
      && (match task.due_date with
         | Some due -> date_le due today_date
         | None -> false))
    tasks

(** Filter tasks due in the future (upcoming) *)
let filter_upcoming tasks =
  let today_date = today () in
  List.filter
    (fun task ->
      (not task.deleted)
      && task.status <> Completed
      && (match task.due_date with
         | Some due -> date_gt due today_date
         | None -> false))
    tasks

(** Filter tasks without a due date (anytime) *)
let filter_anytime tasks =
  List.filter
    (fun task ->
      (not task.deleted)
      && task.status <> Completed
      && task.status <> Inbox
      && Option.is_none task.due_date)
    tasks

(** Filter completed tasks *)
let filter_completed tasks =
  List.filter (fun task -> (not task.deleted) && task.status = Completed) tasks

(** Filter overdue tasks (for review) *)
let filter_review tasks =
  let today_date = today () in
  List.filter
    (fun task ->
      (not task.deleted)
      && task.status <> Completed
      && (match task.due_date with
         | Some due -> date_lt due today_date
         | None -> false))
    tasks

(** Filter tasks by project *)
let filter_by_project tasks project_id =
  List.filter
    (fun task ->
      (not task.deleted)
      && task.status <> Completed
      && (match task.project_id with
         | Some pid -> pid = project_id
         | None -> false))
    tasks

(** Filter tasks by tag *)
let filter_by_tag tasks tag_id =
  List.filter
    (fun task ->
      (not task.deleted)
      && task.status <> Completed
      && List.mem tag_id task.tags)
    tasks

(** Helper: check if s1 is a substring of s2 *)
let is_substring sub str =
  let sub_len = String.length sub in
  let str_len = String.length str in
  if sub_len > str_len then false
  else
    let rec check i =
      if i + sub_len > str_len then false
      else if String.sub str i sub_len = sub then true
      else check (i + 1)
    in
    check 0

(** Search tasks by query (case-insensitive in title and notes) *)
let search_tasks tasks query =
  let query_lower = String.lowercase_ascii query in
  List.filter
    (fun task ->
      (not task.deleted)
      && (is_substring query_lower (String.lowercase_ascii task.title)
         || match task.notes with
            | Some notes -> is_substring query_lower (String.lowercase_ascii notes)
            | None -> false))
    tasks

(** Sort tasks by order_index, then created_at *)
let sort_by_order tasks =
  List.sort
    (fun a b ->
      let cmp = Int64.compare a.order_index b.order_index in
      if cmp <> 0 then cmp else Ptime.compare b.created_at a.created_at)
    tasks

(** Sort tasks by due date (earliest first), nulls last *)
let sort_by_due_date tasks =
  List.sort
    (fun a b ->
      match (a.due_date, b.due_date) with
      | None, None -> 0
      | None, Some _ -> 1
      | Some _, None -> -1
      | Some d1, Some d2 -> compare_date d1 d2)
    tasks

(** Sort tasks by priority (highest first) *)
let sort_by_priority tasks =
  let priority_order = function
    | High -> 0
    | Medium -> 1
    | Low -> 2
    | None -> 3
  in
  List.sort
    (fun a b -> Int.compare (priority_order a.priority) (priority_order b.priority))
    tasks

(** Count tasks by filter *)
let count_inbox tasks = List.length (filter_inbox tasks)
let count_today tasks = List.length (filter_today tasks)
let count_upcoming tasks = List.length (filter_upcoming tasks)
let count_anytime tasks = List.length (filter_anytime tasks)
let count_completed tasks = List.length (filter_completed tasks)
let count_review tasks = List.length (filter_review tasks)

let count_by_project tasks project_id =
  List.length (filter_by_project tasks project_id)

let count_by_tag tasks tag_id = List.length (filter_by_tag tasks tag_id)
