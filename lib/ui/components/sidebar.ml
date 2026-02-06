(** Sidebar navigation component *)

(** Sidebar item types *)
type sidebar_item =
  | Inbox
  | Today
  | Upcoming
  | Anytime
  | Completed
  | Review
  | GitHub
  | Toggl
  | Settings
  | ProjectItem of string  (* project_id *)
  | TagItem of string      (* tag_id *)

(** Sidebar section *)
type sidebar_section =
  | Views
  | Projects
  | Tags
  | Integrations

(** Sidebar state *)
type t = {
  selected_index : int;
  items : (sidebar_item * int) list;  (* item, count *)
  focused : bool;
  projects : Project.t list;
  tags : Tag.t list;
  project_counts : (string, int) Hashtbl.t;
  tag_counts : (string, int) Hashtbl.t;
  show_github : bool;
  show_toggl : bool;
}

(** Sidebar counts type - must be defined before build_items *)
type sidebar_counts = {
  inbox : int;
  today : int;
  upcoming : int;
  anytime : int;
  completed : int;
  review : int;
}

(** Create initial sidebar state *)
let create ?(show_github = false) ?(show_toggl = false) () =
  {
    selected_index = 0;
    items = [];
    focused = true;
    projects = [];
    tags = [];
    project_counts = Hashtbl.create 16;
    tag_counts = Hashtbl.create 16;
    show_github;
    show_toggl;
  }

(** Build the items list from current state *)
let build_items sidebar (counts : sidebar_counts) =
  let items = ref [] in
  (* Main views *)
  items := !items @ [
    (Inbox, counts.inbox);
    (Today, counts.today);
    (Upcoming, counts.upcoming);
    (Anytime, counts.anytime);
    (Completed, counts.completed);
    (Review, counts.review);
  ];
  (* Integrations *)
  if sidebar.show_github then
    items := !items @ [(GitHub, 0)];
  if sidebar.show_toggl then
    items := !items @ [(Toggl, 0)];
  (* Settings *)
  items := !items @ [(Settings, 0)];
  (* Projects *)
  List.iter
    (fun (project : Project.t) ->
      let count =
        try Hashtbl.find sidebar.project_counts project.id
        with Not_found -> 0
      in
      items := !items @ [(ProjectItem project.id, count)])
    (List.filter (fun (p : Project.t) -> not p.is_inbox) sidebar.projects);
  (* Tags *)
  List.iter
    (fun (tag : Tag.t) ->
      let count =
        try Hashtbl.find sidebar.tag_counts tag.id
        with Not_found -> 0
      in
      items := !items @ [(TagItem tag.id, count)])
    sidebar.tags;
  !items

(** Update counts for sidebar items *)
let update_counts sidebar counts =
  let items = build_items sidebar counts in
  { sidebar with items }

(** Update projects list *)
let set_projects sidebar projects project_counts =
  let () = Hashtbl.clear sidebar.project_counts in
  List.iter
    (fun (pid, count) -> Hashtbl.add sidebar.project_counts pid count)
    project_counts;
  { sidebar with projects }

(** Update tags list *)
let set_tags sidebar tags tag_counts =
  let () = Hashtbl.clear sidebar.tag_counts in
  List.iter
    (fun (tid, count) -> Hashtbl.add sidebar.tag_counts tid count)
    tag_counts;
  { sidebar with tags }

(** Get the currently selected item *)
let selected_item sidebar =
  if sidebar.selected_index >= 0 && sidebar.selected_index < List.length sidebar.items then
    Some (fst (List.nth sidebar.items sidebar.selected_index))
  else
    None

(** Move selection up *)
let select_previous sidebar =
  let new_index =
    if sidebar.selected_index > 0 then sidebar.selected_index - 1
    else List.length sidebar.items - 1
  in
  { sidebar with selected_index = new_index }

(** Move selection down *)
let select_next sidebar =
  let new_index =
    if sidebar.selected_index < List.length sidebar.items - 1 then
      sidebar.selected_index + 1
    else 0
  in
  { sidebar with selected_index = new_index }

(** Jump to first item *)
let select_first sidebar = { sidebar with selected_index = 0 }

(** Jump to last item *)
let select_last sidebar =
  let last = max 0 (List.length sidebar.items - 1) in
  { sidebar with selected_index = last }

(** Set focus state *)
let set_focused sidebar focused = { sidebar with focused }

(** Get item label *)
let item_label sidebar = function
  | Inbox -> Theme.Icon.inbox ^ " Inbox"
  | Today -> Theme.Icon.today ^ " Today"
  | Upcoming -> Theme.Icon.upcoming ^ " Upcoming"
  | Anytime -> Theme.Icon.anytime ^ " Anytime"
  | Completed -> Theme.Icon.completed ^ " Completed"
  | Review -> Theme.Icon.review ^ " Review"
  | GitHub -> Theme.Icon.github ^ " GitHub"
  | Toggl -> Theme.Icon.toggl ^ " Toggl"
  | Settings -> Theme.Icon.settings ^ " Settings"
  | ProjectItem pid -> (
      match List.find_opt (fun (p : Project.t) -> p.id = pid) sidebar.projects with
      | Some project ->
          let icon = Option.value ~default:Theme.Icon.project project.icon in
          icon ^ " " ^ project.name
      | None -> Theme.Icon.project ^ " Unknown")
  | TagItem tid -> (
      match List.find_opt (fun (t : Tag.t) -> t.id = tid) sidebar.tags with
      | Some tag -> Theme.Icon.tag ^ " " ^ tag.name
      | None -> Theme.Icon.tag ^ " Unknown")

(** Render the sidebar *)
let render sidebar width =
  let lines = ref [] in
  let selected_bg = if sidebar.focused then Theme.Color.bg_selected else "" in

  (* Title *)
  lines := !lines @ [Theme.bold (Theme.colored Theme.Color.primary "phitodo")];
  lines := !lines @ [String.make width '-'];

  (* Items *)
  List.iteri
    (fun i (item, count) ->
      let label = item_label sidebar item in
      let count_str = if count > 0 then Printf.sprintf " (%d)" count else "" in
      let line = label ^ count_str in
      let padded = Theme.pad_right width line in
      let styled =
        if i = sidebar.selected_index then
          selected_bg ^ Theme.bold padded ^ Theme.Color.reset
        else padded
      in
      lines := !lines @ [styled])
    sidebar.items;

  !lines
