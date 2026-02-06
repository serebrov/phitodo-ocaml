(** Task list component *)

(** Task list state *)
type t = {
  tasks : Task.t list;
  selected_index : int;
  focused : bool;
  title : string;
  scroll_offset : int;
  visible_height : int;
}

(** Create initial task list state *)
let create ~title ?(visible_height = 20) () =
  {
    tasks = [];
    selected_index = 0;
    focused = false;
    title;
    scroll_offset = 0;
    visible_height;
  }

(** Update the task list *)
let set_tasks list tasks =
  let selected_index =
    if list.selected_index >= List.length tasks then
      max 0 (List.length tasks - 1)
    else list.selected_index
  in
  { list with tasks; selected_index }

(** Set the visible height for scrolling *)
let set_visible_height list height =
  { list with visible_height = height }

(** Get the currently selected task *)
let selected_task list =
  if list.selected_index >= 0 && list.selected_index < List.length list.tasks then
    Some (List.nth list.tasks list.selected_index)
  else None

(** Get selected task index *)
let selected_index list = list.selected_index

(** Adjust scroll offset to keep selection visible *)
let adjust_scroll list =
  let scroll_offset =
    if list.selected_index < list.scroll_offset then list.selected_index
    else if list.selected_index >= list.scroll_offset + list.visible_height then
      list.selected_index - list.visible_height + 1
    else list.scroll_offset
  in
  { list with scroll_offset }

(** Move selection up *)
let select_previous list =
  let new_index =
    if list.selected_index > 0 then list.selected_index - 1
    else List.length list.tasks - 1
  in
  adjust_scroll { list with selected_index = new_index }

(** Move selection down *)
let select_next list =
  let new_index =
    if list.selected_index < List.length list.tasks - 1 then
      list.selected_index + 1
    else 0
  in
  adjust_scroll { list with selected_index = new_index }

(** Jump to first item *)
let select_first list = adjust_scroll { list with selected_index = 0 }

(** Jump to last item *)
let select_last list =
  let last = max 0 (List.length list.tasks - 1) in
  adjust_scroll { list with selected_index = last }

(** Set focus state *)
let set_focused list focused = { list with focused }

(** Format a single task row *)
let format_task_row width task is_selected is_focused =
  let checkbox = Theme.checkbox (task.Task.status = Task.Completed) in
  let priority = Theme.format_priority task.priority in
  let kind = Theme.format_kind task.kind in
  let title = Theme.truncate (width - 20) task.title in
  let due_date = Theme.format_due_date task in

  (* Build the row *)
  let row =
    Printf.sprintf "%s %s%s %s %s" checkbox priority kind title due_date
  in
  let padded = Theme.pad_right width row in

  (* Apply selection styling *)
  if is_selected then
    let bg = if is_focused then Theme.Color.bg_selected else Theme.Color.bg_hover in
    bg ^ Theme.bold padded ^ Theme.Color.reset
  else padded

(** Render the task list *)
let render list width =
  let lines = ref [] in

  (* Title *)
  let title_line =
    Theme.bold (Theme.colored Theme.Color.primary list.title)
    ^ Printf.sprintf " (%d)" (List.length list.tasks)
  in
  lines := !lines @ [Theme.pad_right width title_line];
  lines := !lines @ [String.make width '-'];

  (* Empty state *)
  if List.length list.tasks = 0 then (
    lines := !lines @ [Theme.dim (Theme.center width "No tasks")];
    !lines)
  else (
    (* Visible tasks *)
    let visible_tasks =
      let rec take_from n count lst =
        if count <= 0 then []
        else
          match lst with
          | [] -> []
          | _ when n > 0 -> take_from (n - 1) count (List.tl lst)
          | x :: xs -> x :: take_from 0 (count - 1) xs
      in
      take_from list.scroll_offset list.visible_height list.tasks
    in

    List.iteri
      (fun i task ->
        let actual_index = list.scroll_offset + i in
        let is_selected = actual_index = list.selected_index in
        let row = format_task_row width task is_selected list.focused in
        lines := !lines @ [row])
      visible_tasks;

    (* Scroll indicator *)
    let total = List.length list.tasks in
    if total > list.visible_height then (
      let scroll_info =
        Printf.sprintf "↑↓ %d-%d of %d"
          (list.scroll_offset + 1)
          (min (list.scroll_offset + list.visible_height) total)
          total
      in
      lines := !lines @ [Theme.dim (Theme.pad_right width scroll_info)]);

    !lines)
