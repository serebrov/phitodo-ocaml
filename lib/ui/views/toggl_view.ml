(** Toggl view - display time entries *)

(** Toggl time entry *)
type time_entry = {
  id : int64;
  description : string option;
  duration : int64;  (* seconds, negative if running *)
  start_time : string;  (* RFC3339 *)
  stop_time : string option;
  project_id : int64 option;
  project_name : string option;
}

(** Toggl view state *)
type t = {
  entries : time_entry list;
  projects : (int64, string) Hashtbl.t;
  selected_index : int;
  loading : bool;
  error : string option;
  days : int;  (* how many days of data to show *)
}

(** Create empty Toggl view *)
let create ?(days = 7) () =
  {
    entries = [];
    projects = Hashtbl.create 16;
    selected_index = 0;
    loading = false;
    error = None;
    days;
  }

(** Set loading state *)
let set_loading view loading = { view with loading; error = None }

(** Set error *)
let set_error view error = { view with error = Some error; loading = false }

(** Set data *)
let set_data view entries projects =
  { view with entries; loading = false; error = None; projects }

(** Get selected entry *)
let selected_entry view =
  if view.selected_index >= 0 && view.selected_index < List.length view.entries then
    Some (List.nth view.entries view.selected_index)
  else None

(** Select next entry *)
let select_next view =
  let new_idx =
    if view.selected_index < List.length view.entries - 1 then
      view.selected_index + 1
    else 0
  in
  { view with selected_index = new_idx }

(** Select previous entry *)
let select_previous view =
  let new_idx =
    if view.selected_index > 0 then view.selected_index - 1
    else max 0 (List.length view.entries - 1)
  in
  { view with selected_index = new_idx }

(** Select first entry *)
let select_first view = { view with selected_index = 0 }

(** Select last entry *)
let select_last view =
  { view with selected_index = max 0 (List.length view.entries - 1) }

(** Format duration in seconds to HH:MM:SS *)
let format_duration secs =
  let secs = if secs < 0L then Int64.neg secs else secs in
  let hours = Int64.div secs 3600L in
  let mins = Int64.div (Int64.rem secs 3600L) 60L in
  let secs = Int64.rem secs 60L in
  Printf.sprintf "%02Ld:%02Ld:%02Ld" hours mins secs

(** Format duration short (2h 30m) *)
let format_duration_short secs =
  let secs = if secs < 0L then Int64.neg secs else secs in
  let hours = Int64.div secs 3600L in
  let mins = Int64.div (Int64.rem secs 3600L) 60L in
  if hours > 0L then Printf.sprintf "%Ldh %Ldm" hours mins
  else Printf.sprintf "%Ldm" mins

(** Get total duration *)
let total_duration view =
  List.fold_left
    (fun acc entry ->
      let dur = if entry.duration < 0L then Int64.neg entry.duration else entry.duration in
      Int64.add acc dur)
    0L view.entries

(** Get duration by project *)
let duration_by_project view =
  let by_project = Hashtbl.create 16 in
  List.iter
    (fun entry ->
      let project = Option.value ~default:"No Project" entry.project_name in
      let dur = if entry.duration < 0L then Int64.neg entry.duration else entry.duration in
      let current = try Hashtbl.find by_project project with Not_found -> 0L in
      Hashtbl.replace by_project project (Int64.add current dur))
    view.entries;
  let items = Hashtbl.fold (fun k v acc -> (k, v) :: acc) by_project [] in
  List.sort (fun (_, a) (_, b) -> Int64.compare b a) items

(** Get duration by date *)
let duration_by_date view =
  let by_date = Hashtbl.create 16 in
  List.iter
    (fun entry ->
      (* Extract date from RFC3339 start_time *)
      let date = String.sub entry.start_time 0 10 in
      let dur = if entry.duration < 0L then Int64.neg entry.duration else entry.duration in
      let current = try Hashtbl.find by_date date with Not_found -> 0L in
      Hashtbl.replace by_date date (Int64.add current dur))
    view.entries;
  let items = Hashtbl.fold (fun k v acc -> (k, v) :: acc) by_date [] in
  List.sort (fun (a, _) (b, _) -> String.compare b a) items  (* Most recent first *)

(** Render a simple bar chart *)
let render_bar max_value value width =
  if max_value = 0L then String.make width ' '
  else
    let filled = Int64.to_int (Int64.div (Int64.mul value (Int64.of_int width)) max_value) in
    let filled = min width filled in
    let bar_char = "#" in
    let empty_char = "-" in
    Theme.colored Theme.Color.primary (String.concat "" (List.init filled (fun _ -> bar_char))) ^
    String.concat "" (List.init (width - filled) (fun _ -> empty_char))

(** Render the Toggl view *)
let render view width =
  let lines = ref [] in

  (* Title *)
  lines := !lines @ [Theme.bold (Theme.colored Theme.Color.primary "Toggl Time Tracking")];
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
        (* Summary *)
        let total = total_duration view in
        lines := !lines @ [
          Printf.sprintf "Total: %s (%d entries in last %d days)"
            (format_duration total)
            (List.length view.entries)
            view.days
        ];
        lines := !lines @ [""];

        (* By project *)
        lines := !lines @ [Theme.bold "By Project:"];
        let by_project = duration_by_project view in
        let max_project_dur =
          List.fold_left (fun acc (_, v) -> max acc v) 0L by_project
        in
        List.iter
          (fun (project, duration) ->
            let bar = render_bar max_project_dur duration 20 in
            let dur_str = format_duration_short duration in
            let line = Printf.sprintf "  %-20s %s %8s" (Theme.truncate 20 project) bar dur_str in
            lines := !lines @ [line])
          (if List.length by_project > 10 then
             List.filteri (fun i _ -> i < 10) by_project
           else by_project);
        lines := !lines @ [""];

        (* By date *)
        lines := !lines @ [Theme.bold "By Date:"];
        let by_date = duration_by_date view in
        let max_date_dur =
          List.fold_left (fun acc (_, v) -> max acc v) 0L by_date
        in
        List.iter
          (fun (date, duration) ->
            let bar = render_bar max_date_dur duration 30 in
            let dur_str = format_duration_short duration in
            let line = Printf.sprintf "  %s %s %8s" date bar dur_str in
            lines := !lines @ [line])
          by_date;
        lines := !lines @ [""];

        (* Recent entries *)
        lines := !lines @ [Theme.bold "Recent Entries:"];
        lines := !lines @ [String.make width '-'];

        if List.length view.entries = 0 then
          lines := !lines @ [Theme.dim (Theme.center width "No time entries")]
        else
          List.iteri
            (fun i entry ->
              if i < 20 then (
                let desc = Option.value ~default:"(no description)" entry.description in
                let project = Option.value ~default:"" entry.project_name in
                let dur =
                  if entry.duration < 0L then
                    Theme.colored Theme.Color.success ("▶ " ^ format_duration entry.duration)
                  else format_duration entry.duration
                in
                let line =
                  Printf.sprintf "%s  %-30s  %s"
                    dur
                    (Theme.truncate 30 desc)
                    (Theme.dim project)
                in
                let padded = Theme.pad_right width line in
                let styled =
                  if i = view.selected_index then
                    Theme.Color.bg_selected ^ Theme.bold padded ^ Theme.Color.reset
                  else padded
                in
                lines := !lines @ [styled]))
            view.entries;

        !lines
