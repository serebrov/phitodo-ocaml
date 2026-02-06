(** Generic task view module used by all task-based views *)

(** Task view state *)
type t = {
  task_list : Task_list.t;
  detail_focused : bool;
}

(** Create a new task view *)
let create ~title () =
  {
    task_list = Task_list.create ~title ();
    detail_focused = false;
  }

(** Update tasks in the view *)
let set_tasks view tasks =
  { view with task_list = Task_list.set_tasks view.task_list tasks }

(** Get currently selected task *)
let selected_task view = Task_list.selected_task view.task_list

(** Focus on list *)
let focus_list view =
  {
    detail_focused = false;
    task_list = Task_list.set_focused view.task_list true;
  }

(** Focus on detail *)
let focus_detail view =
  {
    detail_focused = true;
    task_list = Task_list.set_focused view.task_list false;
  }

(** Cycle focus between list and detail *)
let cycle_focus view =
  if view.detail_focused then focus_list view else focus_detail view

(** Set overall focus state *)
let set_focused view focused =
  if focused then
    if view.detail_focused then focus_detail view else focus_list view
  else
    { view with task_list = Task_list.set_focused view.task_list false }

(** Is detail focused? *)
let is_detail_focused view = view.detail_focused

(** Navigation: select previous *)
let select_previous view =
  { view with task_list = Task_list.select_previous view.task_list }

(** Navigation: select next *)
let select_next view =
  { view with task_list = Task_list.select_next view.task_list }

(** Navigation: select first *)
let select_first view =
  { view with task_list = Task_list.select_first view.task_list }

(** Navigation: select last *)
let select_last view =
  { view with task_list = Task_list.select_last view.task_list }

(** Set visible height *)
let set_visible_height view height =
  { view with task_list = Task_list.set_visible_height view.task_list height }

(** Render the task view *)
let render view list_width detail_width =
  let list_lines = Task_list.render view.task_list list_width in
  let detail_lines =
    Task_detail.render (selected_task view) detail_width
  in
  (list_lines, detail_lines)
