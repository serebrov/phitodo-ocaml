(** GitHub view - display issues, PRs, and review requests *)

(** GitHub item type *)
type github_item = {
  id : int64;
  number : int64;
  title : string;
  html_url : string;
  state : string;
  body : string option;
  repository : string;
  user : string option;
  is_pr : bool;
}

(** GitHub column *)
type github_column =
  | ReviewRequests
  | MyPRs
  | AssignedIssues

(** GitHub view state *)
type t = {
  review_prs : github_item list;
  my_prs : github_item list;
  assigned_issues : github_item list;
  selected_column : github_column;
  selected_indices : int * int * int;  (* review, my_prs, assigned *)
  loading : bool;
  error : string option;
}

(** Create empty GitHub view *)
let create () =
  {
    review_prs = [];
    my_prs = [];
    assigned_issues = [];
    selected_column = ReviewRequests;
    selected_indices = (0, 0, 0);
    loading = false;
    error = None;
  }

(** Set loading state *)
let set_loading view loading = { view with loading; error = None }

(** Set error *)
let set_error view error = { view with error = Some error; loading = false }

(** Set data *)
let set_data view ~review_prs ~my_prs ~assigned_issues =
  { view with review_prs; my_prs; assigned_issues; loading = false; error = None }

(** Get current column's items *)
let current_items view =
  match view.selected_column with
  | ReviewRequests -> view.review_prs
  | MyPRs -> view.my_prs
  | AssignedIssues -> view.assigned_issues

(** Get current column's selected index *)
let current_index view =
  let r, m, a = view.selected_indices in
  match view.selected_column with
  | ReviewRequests -> r
  | MyPRs -> m
  | AssignedIssues -> a

(** Set current column's selected index *)
let set_current_index view idx =
  let r, m, a = view.selected_indices in
  let indices =
    match view.selected_column with
    | ReviewRequests -> (idx, m, a)
    | MyPRs -> (r, idx, a)
    | AssignedIssues -> (r, m, idx)
  in
  { view with selected_indices = indices }

(** Get selected item *)
let selected_item view =
  let items = current_items view in
  let idx = current_index view in
  if idx >= 0 && idx < List.length items then
    Some (List.nth items idx)
  else None

(** Move to next column *)
let next_column view =
  let col =
    match view.selected_column with
    | ReviewRequests -> MyPRs
    | MyPRs -> AssignedIssues
    | AssignedIssues -> ReviewRequests
  in
  { view with selected_column = col }

(** Move to previous column *)
let prev_column view =
  let col =
    match view.selected_column with
    | ReviewRequests -> AssignedIssues
    | MyPRs -> ReviewRequests
    | AssignedIssues -> MyPRs
  in
  { view with selected_column = col }

(** Select next item *)
let select_next view =
  let items = current_items view in
  let idx = current_index view in
  let new_idx =
    if idx < List.length items - 1 then idx + 1 else 0
  in
  set_current_index view new_idx

(** Select previous item *)
let select_previous view =
  let items = current_items view in
  let idx = current_index view in
  let new_idx =
    if idx > 0 then idx - 1 else max 0 (List.length items - 1)
  in
  set_current_index view new_idx

(** Select first item *)
let select_first view = set_current_index view 0

(** Select last item *)
let select_last view =
  let items = current_items view in
  set_current_index view (max 0 (List.length items - 1))

(** Column label *)
let column_label = function
  | ReviewRequests -> "Review Requests"
  | MyPRs -> "My PRs"
  | AssignedIssues -> "Assigned Issues"

(** Render a column *)
let render_column items selected_idx is_focused width =
  let lines = ref [] in
  if List.length items = 0 then
    lines := !lines @ [Theme.dim (Theme.center width "No items")]
  else
    List.iteri
      (fun i item ->
        let prefix = if item.is_pr then "[PR]" else "[ISS]" in
        let num = Printf.sprintf "#%Ld" item.number in
        let title = Theme.truncate (width - 15) item.title in
        let line = Printf.sprintf "%s %s %s" prefix num title in
        let padded = Theme.pad_right width line in
        let styled =
          if i = selected_idx && is_focused then
            Theme.Color.bg_selected ^ Theme.bold padded ^ Theme.Color.reset
          else if i = selected_idx then
            Theme.Color.bg_hover ^ padded ^ Theme.Color.reset
          else padded
        in
        lines := !lines @ [styled])
      items;
  !lines

(** Render the GitHub view *)
let render view width =
  let lines = ref [] in

  (* Title *)
  lines := !lines @ [Theme.bold (Theme.colored Theme.Color.primary "GitHub")];
  lines := !lines @ [String.make width '-'];

  (* Loading/Error state *)
  if view.loading then (
    lines := !lines @ [Theme.dim (Theme.center width "Loading...")];
    !lines)
  else
    match view.error with
    | Some err ->
        lines := !lines @ [Theme.colored Theme.Color.error err];
        !lines
    | None ->
        (* Column headers *)
        let col_width = (width - 4) / 3 in
        let header1 =
          let label = column_label ReviewRequests in
          let count = Printf.sprintf " (%d)" (List.length view.review_prs) in
          if view.selected_column = ReviewRequests then
            Theme.bold (Theme.pad_right col_width (label ^ count))
          else Theme.pad_right col_width (label ^ count)
        in
        let header2 =
          let label = column_label MyPRs in
          let count = Printf.sprintf " (%d)" (List.length view.my_prs) in
          if view.selected_column = MyPRs then
            Theme.bold (Theme.pad_right col_width (label ^ count))
          else Theme.pad_right col_width (label ^ count)
        in
        let header3 =
          let label = column_label AssignedIssues in
          let count = Printf.sprintf " (%d)" (List.length view.assigned_issues) in
          if view.selected_column = AssignedIssues then
            Theme.bold (Theme.pad_right col_width (label ^ count))
          else Theme.pad_right col_width (label ^ count)
        in
        lines := !lines @ [header1 ^ " | " ^ header2 ^ " | " ^ header3];
        lines := !lines @ [String.make width '-'];

        (* Column content *)
        let r, m, a = view.selected_indices in
        let col1 = render_column view.review_prs r (view.selected_column = ReviewRequests) col_width in
        let col2 = render_column view.my_prs m (view.selected_column = MyPRs) col_width in
        let col3 = render_column view.assigned_issues a (view.selected_column = AssignedIssues) col_width in

        (* Combine columns row by row *)
        let max_rows = max (List.length col1) (max (List.length col2) (List.length col3)) in
        for i = 0 to max_rows - 1 do
          let c1 = if i < List.length col1 then List.nth col1 i else String.make col_width ' ' in
          let c2 = if i < List.length col2 then List.nth col2 i else String.make col_width ' ' in
          let c3 = if i < List.length col3 then List.nth col3 i else String.make col_width ' ' in
          lines := !lines @ [c1 ^ " | " ^ c2 ^ " | " ^ c3]
        done;

        (* Selected item details *)
        (match selected_item view with
        | Some item ->
            lines := !lines @ [""];
            lines := !lines @ [String.make width '-'];
            lines := !lines @ [Theme.bold item.title];
            lines := !lines @ [Theme.dim (Printf.sprintf "%s #%Ld" item.repository item.number)];
            (match item.body with
            | Some body when String.length body > 0 ->
                lines := !lines @ [""];
                let body_lines = String.split_on_char '\n' body in
                List.iter
                  (fun line -> lines := !lines @ [Theme.truncate width line])
                  (if List.length body_lines > 5 then
                     List.filteri (fun i _ -> i < 5) body_lines @ ["..."]
                   else body_lines)
            | _ -> ());
            lines := !lines @ [""];
            lines := !lines @ [Theme.colored Theme.Color.primary item.html_url]
        | None -> ());

        !lines
