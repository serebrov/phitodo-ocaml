(** Configuration management using TOML *)

type t = {
  shortcut_modifier : string;
  github_token : string option;
  github_repos : string list;
  toggl_token : string option;
  toggl_hidden_projects : string list;
}

let default =
  {
    shortcut_modifier = "alt";
    github_token = None;
    github_repos = [];
    toggl_token = None;
    toggl_hidden_projects = [];
  }

(** Get the config directory path *)
let config_dir () =
  let home =
    try Sys.getenv "HOME"
    with Not_found -> failwith "HOME environment variable not set"
  in
  Filename.concat home ".config/phitodo-tui"

(** Get the data directory path *)
let data_dir () =
  let home =
    try Sys.getenv "HOME"
    with Not_found -> failwith "HOME environment variable not set"
  in
  Filename.concat home ".local/share/phitodo-tui"

(** Get the config file path *)
let config_path () = Filename.concat (config_dir ()) "config.toml"

(** Get the database path *)
let database_path () = Filename.concat (data_dir ()) "phitodo.db"

(** Create directory if it doesn't exist *)
let ensure_dir path =
  if not (Sys.file_exists path) then
    let rec mkdir_p path =
      let parent = Filename.dirname path in
      if parent <> path && not (Sys.file_exists parent) then mkdir_p parent;
      if not (Sys.file_exists path) then Unix.mkdir path 0o755
    in
    mkdir_p path

(** Parse config from TOML *)
let of_toml toml =
  let open Otoml in
  let shortcut_modifier =
    find_opt toml (get_string ~strict:false) [ "shortcut_modifier" ]
    |> Option.value ~default:"alt"
  in
  let github_token =
    find_opt toml (get_string ~strict:false) [ "github_token" ]
  in
  let github_repos =
    find_opt toml (get_array (get_string ~strict:false)) [ "github_repos" ]
    |> Option.value ~default:[]
  in
  let toggl_token =
    find_opt toml (get_string ~strict:false) [ "toggl_token" ]
  in
  let toggl_hidden_projects =
    find_opt toml
      (get_array (get_string ~strict:false))
      [ "toggl_hidden_projects" ]
    |> Option.value ~default:[]
  in
  {
    shortcut_modifier;
    github_token;
    github_repos;
    toggl_token;
    toggl_hidden_projects;
  }

(** Convert config to TOML *)
let to_toml config =
  let table = Otoml.table [] in
  let table =
    Otoml.update table [ "shortcut_modifier" ]
      (Some (Otoml.string config.shortcut_modifier))
  in
  let table =
    match config.github_token with
    | Some token ->
        Otoml.update table [ "github_token" ] (Some (Otoml.string token))
    | None -> table
  in
  let table =
    if config.github_repos <> [] then
      Otoml.update table [ "github_repos" ]
        (Some (Otoml.array (List.map Otoml.string config.github_repos)))
    else table
  in
  let table =
    match config.toggl_token with
    | Some token ->
        Otoml.update table [ "toggl_token" ] (Some (Otoml.string token))
    | None -> table
  in
  let table =
    if config.toggl_hidden_projects <> [] then
      Otoml.update table [ "toggl_hidden_projects" ]
        (Some
           (Otoml.array (List.map Otoml.string config.toggl_hidden_projects)))
    else table
  in
  table

(** Load config from file, creating default if not exists *)
let load () =
  let path = config_path () in
  ensure_dir (config_dir ());
  if Sys.file_exists path then
    try
      let toml = Otoml.Parser.from_file path in
      of_toml toml
    with _ -> default
  else default

(** Save config to file *)
let save config =
  let path = config_path () in
  ensure_dir (config_dir ());
  let toml = to_toml config in
  let content = Otoml.Printer.to_string toml in
  let oc = open_out path in
  output_string oc content;
  close_out oc

(** Check if GitHub is configured *)
let has_github config =
  match config.github_token with
  | Some token -> String.length token > 0
  | None -> false

(** Check if Toggl is configured *)
let has_toggl config =
  match config.toggl_token with
  | Some token -> String.length token > 0
  | None -> false

(** Ensure data directory exists and return database path *)
let ensure_database () =
  ensure_dir (data_dir ());
  database_path ()
