(** Tag view - tasks filtered by tag *)

type t = {
  task_view : Task_view.t;
  tag_id : string;
  tag_name : string;
}

let create ~tag_id ~tag_name () =
  {
    task_view = Task_view.create ~title:tag_name ();
    tag_id;
    tag_name;
  }

let set_tasks view tasks =
  let filtered = Filter_service.filter_by_tag tasks view.tag_id in
  { view with task_view = Task_view.set_tasks view.task_view filtered }

let set_tag _view ~tag_id ~tag_name =
  let task_view = Task_view.create ~title:tag_name () in
  { task_view; tag_id; tag_name }

let selected_task view = Task_view.selected_task view.task_view
let focus_list view = { view with task_view = Task_view.focus_list view.task_view }
let focus_detail view = { view with task_view = Task_view.focus_detail view.task_view }
let cycle_focus view = { view with task_view = Task_view.cycle_focus view.task_view }
let set_focused view focused = { view with task_view = Task_view.set_focused view.task_view focused }
let is_detail_focused view = Task_view.is_detail_focused view.task_view
let select_previous view = { view with task_view = Task_view.select_previous view.task_view }
let select_next view = { view with task_view = Task_view.select_next view.task_view }
let select_first view = { view with task_view = Task_view.select_first view.task_view }
let select_last view = { view with task_view = Task_view.select_last view.task_view }
let set_visible_height view height = { view with task_view = Task_view.set_visible_height view.task_view height }

let render view list_width detail_width =
  Task_view.render view.task_view list_width detail_width
