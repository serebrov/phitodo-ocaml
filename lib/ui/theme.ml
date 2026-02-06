(** Theme and styling for the TUI *)

(** ANSI color codes for terminal styling *)
module Color = struct
  (* Base colors *)
  let white = "\027[97m"
  let black = "\027[30m"
  let gray = "\027[90m"
  let light_gray = "\027[37m"

  (* Theme colors *)
  let primary = "\027[38;2;0;90;180m"     (* Strong blue *)
  let success = "\027[38;2;20;140;50m"    (* Green *)
  let error = "\027[38;2;190;30;30m"      (* Red *)
  let warning = "\027[38;2;180;120;0m"    (* Orange *)

  (* Priority colors *)
  let priority_high = "\027[38;2;190;30;30m"    (* Red *)
  let priority_medium = "\027[38;2;180;120;0m"  (* Orange *)
  let priority_low = "\027[38;2;20;140;50m"     (* Green *)
  let priority_none = "\027[90m"                (* Gray *)

  (* Status colors *)
  let status_inbox = "\027[38;2;100;100;100m"   (* Gray *)
  let status_active = "\027[38;2;0;90;180m"     (* Blue *)
  let status_scheduled = "\027[38;2;150;100;200m"  (* Purple *)
  let status_completed = "\027[38;2;20;140;50m" (* Green *)
  let status_cancelled = "\027[38;2;150;150;150m"  (* Light gray *)

  (* Background colors *)
  let bg_white = "\027[107m"
  let bg_black = "\027[40m"
  let bg_primary = "\027[48;2;0;90;180m"
  let bg_selected = "\027[48;2;230;230;230m"
  let bg_hover = "\027[48;2;245;245;245m"

  (* Styles *)
  let bold = "\027[1m"
  let dim = "\027[2m"
  let italic = "\027[3m"
  let underline = "\027[4m"
  let inverse = "\027[7m"
  let reset = "\027[0m"
end

(** Apply a color to text *)
let colored color text = color ^ text ^ Color.reset

(** Apply bold styling *)
let bold text = Color.bold ^ text ^ Color.reset

(** Apply dim styling *)
let dim text = Color.dim ^ text ^ Color.reset

(** Apply inverse styling (highlighted) *)
let inverse text = Color.inverse ^ text ^ Color.reset

(** Get priority color *)
let priority_color = function
  | Task.High -> Color.priority_high
  | Task.Medium -> Color.priority_medium
  | Task.Low -> Color.priority_low
  | Task.None -> Color.priority_none

(** Get status color *)
let status_color = function
  | Task.Inbox -> Color.status_inbox
  | Task.Active -> Color.status_active
  | Task.Scheduled -> Color.status_scheduled
  | Task.Completed -> Color.status_completed
  | Task.Cancelled -> Color.status_cancelled

(** Format priority with color *)
let format_priority priority =
  let symbol = Task.task_priority_symbol priority in
  colored (priority_color priority) symbol

(** Format status with color *)
let format_status status =
  let text = Task.task_status_display status in
  colored (status_color status) text

(** Format a task kind badge *)
let format_kind = function
  | Some kind -> colored Color.gray (Task.task_kind_symbol kind)
  | None -> ""

(** Format a task size badge *)
let format_size = function
  | Some size -> colored Color.gray ("[" ^ Task.task_size_display size ^ "]")
  | None -> ""

(** Format due date with color (red if overdue, orange if today) *)
let format_due_date task =
  match task.Task.due_date with
  | None -> ""
  | Some (y, m, d) ->
      let date_str = Printf.sprintf "%04d-%02d-%02d" y m d in
      if Task.is_overdue task then colored Color.error date_str
      else if Task.is_due_today task then colored Color.warning date_str
      else colored Color.gray date_str

(** Checkbox display *)
let checkbox completed =
  if completed then colored Color.success "[x]"
  else colored Color.gray "[ ]"

(** Sidebar icons *)
module Icon = struct
  let inbox = "📥"
  let today = "📅"
  let upcoming = "📆"
  let anytime = "💭"
  let completed = "✅"
  let review = "🔍"
  let github = "🐙"
  let toggl = "⏱️"
  let settings = "⚙️"
  let project = "📁"
  let tag = "🏷️"
  let help = "❓"
end

(** Box drawing characters *)
module Box = struct
  let horizontal = "─"
  let vertical = "│"
  let top_left = "┌"
  let top_right = "┐"
  let bottom_left = "└"
  let bottom_right = "┘"
  let t_down = "┬"
  let t_up = "┴"
  let t_right = "├"
  let t_left = "┤"
  let cross = "┼"
end

(** Create a horizontal line *)
let hline width = String.make width '-'

(** Calculate visual width of a string, accounting for ANSI codes and Unicode *)
let visual_width str =
  let len = String.length str in
  let rec loop i width in_escape =
    if i >= len then width
    else
      let c = String.get str i in
      if in_escape then
        (* Inside ANSI escape sequence, wait for 'm' *)
        if c = 'm' then loop (i + 1) width false
        else loop (i + 1) width true
      else if c = '\027' then
        (* Start of ANSI escape sequence *)
        loop (i + 1) width true
      else if Char.code c land 0x80 = 0 then
        (* ASCII character - width 1 *)
        loop (i + 1) (width + 1) false
      else if Char.code c land 0xE0 = 0xC0 then
        (* 2-byte UTF-8 sequence - width 1 *)
        loop (i + 2) (width + 1) false
      else if Char.code c land 0xF0 = 0xE0 then
        (* 3-byte UTF-8 sequence - likely emoji, width 2 *)
        loop (i + 3) (width + 2) false
      else if Char.code c land 0xF8 = 0xF0 then
        (* 4-byte UTF-8 sequence - emoji, width 2 *)
        loop (i + 4) (width + 2) false
      else
        (* Continuation byte or unknown - skip *)
        loop (i + 1) width false
  in
  loop 0 0 false

(** Pad string to width (visually) *)
let pad_right width str =
  let vw = visual_width str in
  if vw >= width then str
  else str ^ String.make (width - vw) ' '

(** Pad string to width (left, visually) *)
let pad_left width str =
  let vw = visual_width str in
  if vw >= width then str
  else String.make (width - vw) ' ' ^ str

(** Center string in width (visually) *)
let center width str =
  let vw = visual_width str in
  if vw >= width then str
  else
    let left = (width - vw) / 2 in
    let right = width - vw - left in
    String.make left ' ' ^ str ^ String.make right ' '

(** Truncate string with ellipsis if too long (by visual width) *)
let truncate width str =
  let vw = visual_width str in
  if vw <= width then str
  else if width <= 3 then String.make width '.'
  else
    (* Simple truncation - just cut bytes, not perfect but works for ASCII *)
    let len = String.length str in
    let rec find_cut i visual_count =
      if i >= len || visual_count >= width - 3 then i
      else
        let c = String.get str i in
        if c = '\027' then
          (* Skip ANSI escape *)
          let rec skip_escape j =
            if j >= len then j
            else if String.get str j = 'm' then j + 1
            else skip_escape (j + 1)
          in
          find_cut (skip_escape (i + 1)) visual_count
        else if Char.code c land 0x80 = 0 then
          find_cut (i + 1) (visual_count + 1)
        else if Char.code c land 0xE0 = 0xC0 then
          find_cut (i + 2) (visual_count + 1)
        else if Char.code c land 0xF0 = 0xE0 then
          find_cut (i + 3) (visual_count + 2)
        else if Char.code c land 0xF8 = 0xF0 then
          find_cut (i + 4) (visual_count + 2)
        else
          find_cut (i + 1) visual_count
    in
    let cut = find_cut 0 0 in
    String.sub str 0 cut ^ "..."

(** Create a bordered box around content *)
let box width height lines =
  let top =
    Box.top_left ^ String.make (width - 2) '-' ^ Box.top_right
  in
  let bottom =
    Box.bottom_left ^ String.make (width - 2) '-' ^ Box.bottom_right
  in
  let middle =
    List.map
      (fun line -> Box.vertical ^ pad_right (width - 2) line ^ Box.vertical)
      lines
  in
  let padding_lines = height - List.length lines - 2 in
  let padding =
    if padding_lines > 0 then
      List.init padding_lines (fun _ ->
          Box.vertical ^ String.make (width - 2) ' ' ^ Box.vertical)
    else []
  in
  top :: middle @ padding @ [ bottom ]
