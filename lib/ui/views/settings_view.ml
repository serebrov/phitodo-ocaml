(** Settings view *)

(** Settings field *)
type settings_field =
  | GithubToken
  | GithubRepos
  | TogglToken
  | TogglHiddenProjects

(** Settings view state *)
type t = {
  config : Config.t;
  selected_field : settings_field;
  editing : bool;
  input : Input.t;
}

(** Create settings view *)
let create config =
  {
    config;
    selected_field = GithubToken;
    editing = false;
    input = Input.create ();
  }

(** Update config *)
let set_config view config = { view with config }

(** Get field value as string *)
let field_value view = function
  | GithubToken -> Option.value ~default:"" view.config.github_token
  | GithubRepos -> String.concat ", " view.config.github_repos
  | TogglToken -> Option.value ~default:"" view.config.toggl_token
  | TogglHiddenProjects -> String.concat ", " view.config.toggl_hidden_projects

(** Set field value from string *)
let set_field_value view field value =
  let config =
    match field with
    | GithubToken ->
        let token = if String.length value > 0 then Some value else None in
        { view.config with github_token = token }
    | GithubRepos ->
        let repos =
          String.split_on_char ',' value
          |> List.map String.trim
          |> List.filter (fun s -> String.length s > 0)
        in
        { view.config with github_repos = repos }
    | TogglToken ->
        let token = if String.length value > 0 then Some value else None in
        { view.config with toggl_token = token }
    | TogglHiddenProjects ->
        let projects =
          String.split_on_char ',' value
          |> List.map String.trim
          |> List.filter (fun s -> String.length s > 0)
        in
        { view.config with toggl_hidden_projects = projects }
  in
  { view with config }

(** Move to next field *)
let next_field = function
  | GithubToken -> GithubRepos
  | GithubRepos -> TogglToken
  | TogglToken -> TogglHiddenProjects
  | TogglHiddenProjects -> GithubToken

(** Move to previous field *)
let prev_field = function
  | GithubToken -> TogglHiddenProjects
  | GithubRepos -> GithubToken
  | TogglToken -> GithubRepos
  | TogglHiddenProjects -> TogglToken

(** Select next field *)
let select_next view =
  { view with selected_field = next_field view.selected_field }

(** Select previous field *)
let select_previous view =
  { view with selected_field = prev_field view.selected_field }

(** Start editing current field *)
let start_edit view =
  let value = field_value view view.selected_field in
  { view with editing = true; input = Input.set_value view.input value }

(** Cancel editing *)
let cancel_edit view =
  { view with editing = false; input = Input.clear view.input }

(** Confirm editing *)
let confirm_edit view =
  let value = Input.value view.input in
  let view = set_field_value view view.selected_field value in
  { view with editing = false; input = Input.clear view.input }

(** Handle input character *)
let input_char view c =
  { view with input = Input.insert_char view.input c }

(** Handle input backspace *)
let input_backspace view =
  { view with input = Input.backspace view.input }

(** Handle input delete *)
let input_delete view =
  { view with input = Input.delete view.input }

(** Handle input cursor left *)
let input_left view =
  { view with input = Input.cursor_left view.input }

(** Handle input cursor right *)
let input_right view =
  { view with input = Input.cursor_right view.input }

(** Field label *)
let field_label = function
  | GithubToken -> "GitHub Token"
  | GithubRepos -> "GitHub Repos"
  | TogglToken -> "Toggl Token"
  | TogglHiddenProjects -> "Hidden Projects"

(** Mask token for display *)
let mask_token s =
  if String.length s > 8 then
    String.sub s 0 4 ^ String.make (String.length s - 8) '*' ^ String.sub s (String.length s - 4) 4
  else if String.length s > 0 then
    String.make (String.length s) '*'
  else "(not set)"

(** Render the settings view *)
let render view width =
  let lines = ref [] in

  (* Title *)
  lines := !lines @ [Theme.bold (Theme.colored Theme.Color.primary "Settings")];
  lines := !lines @ [String.make width '-'];
  lines := !lines @ [""];

  (* GitHub section *)
  lines := !lines @ [Theme.bold "GitHub Integration"];
  lines := !lines @ [""];

  (* GitHub Token *)
  let is_selected = view.selected_field = GithubToken in
  let is_editing = is_selected && view.editing in
  let prefix = if is_selected then "> " else "  " in
  let value =
    if is_editing then Input.render (Input.set_focused view.input true) (width - 20)
    else mask_token (field_value view GithubToken)
  in
  let line = Printf.sprintf "%s%-18s: %s" prefix (field_label GithubToken) value in
  lines := !lines @ [if is_selected then Theme.bold line else line];

  (* GitHub Repos *)
  let is_selected = view.selected_field = GithubRepos in
  let is_editing = is_selected && view.editing in
  let prefix = if is_selected then "> " else "  " in
  let value =
    if is_editing then Input.render (Input.set_focused view.input true) (width - 20)
    else
      let repos = field_value view GithubRepos in
      if String.length repos = 0 then "(none)" else repos
  in
  let line = Printf.sprintf "%s%-18s: %s" prefix (field_label GithubRepos) value in
  lines := !lines @ [if is_selected then Theme.bold line else line];

  lines := !lines @ [""];

  (* Toggl section *)
  lines := !lines @ [Theme.bold "Toggl Integration"];
  lines := !lines @ [""];

  (* Toggl Token *)
  let is_selected = view.selected_field = TogglToken in
  let is_editing = is_selected && view.editing in
  let prefix = if is_selected then "> " else "  " in
  let value =
    if is_editing then Input.render (Input.set_focused view.input true) (width - 20)
    else mask_token (field_value view TogglToken)
  in
  let line = Printf.sprintf "%s%-18s: %s" prefix (field_label TogglToken) value in
  lines := !lines @ [if is_selected then Theme.bold line else line];

  (* Toggl Hidden Projects *)
  let is_selected = view.selected_field = TogglHiddenProjects in
  let is_editing = is_selected && view.editing in
  let prefix = if is_selected then "> " else "  " in
  let value =
    if is_editing then Input.render (Input.set_focused view.input true) (width - 20)
    else
      let projects = field_value view TogglHiddenProjects in
      if String.length projects = 0 then "(none)" else projects
  in
  let line = Printf.sprintf "%s%-18s: %s" prefix (field_label TogglHiddenProjects) value in
  lines := !lines @ [if is_selected then Theme.bold line else line];

  lines := !lines @ [""];
  lines := !lines @ [Theme.dim "j/k: Navigate  Enter: Edit  Esc: Cancel"];

  !lines
