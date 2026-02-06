(** Help overlay component *)

(** Help section *)
type help_section = {
  title : string;
  items : (string * string) list;  (* key, description *)
}

(** All help sections *)
let sections = [
  {
    title = "Navigation";
    items = [
      ("1-9", "Switch views");
      ("Tab", "Cycle focus");
      ("Shift+Tab", "Reverse cycle");
      ("h/l or ←/→", "Focus left/right");
      ("j/k or ↓/↑", "Navigate list");
      ("g", "Jump to first");
      ("G", "Jump to last");
    ];
  };
  {
    title = "Task Actions";
    items = [
      ("Space", "Toggle complete");
      ("n", "New task");
      ("N", "New project");
      ("e", "Edit task");
      ("d", "Delete task");
      ("o", "Open URL");
    ];
  };
  {
    title = "Status";
    items = [
      ("i", "Set to Inbox");
      ("a", "Set to Active");
      ("s", "Set to Scheduled");
    ];
  };
  {
    title = "Priority";
    items = [
      ("Alt+1", "None");
      ("Alt+2", "Low");
      ("Alt+3", "Medium");
      ("Alt+4", "High");
    ];
  };
  {
    title = "Other";
    items = [
      ("/", "Search");
      ("r", "Refresh");
      ("?", "Help");
      ("q", "Quit");
    ];
  };
]

(** Render the help overlay *)
let render width height =
  let lines = ref [] in
  let inner_width = width - 4 in

  (* Top border *)
  lines := !lines @ [
    Theme.Box.top_left ^ String.make (width - 2) '-' ^ Theme.Box.top_right
  ];

  (* Title *)
  lines := !lines @ [
    Theme.Box.vertical ^ " " ^
    Theme.bold (Theme.colored Theme.Color.primary
      (Theme.center (inner_width - 1) "Keyboard Shortcuts")) ^
    Theme.Box.vertical
  ];

  (* Separator *)
  lines := !lines @ [
    Theme.Box.t_right ^ String.make (width - 2) '-' ^ Theme.Box.t_left
  ];

  (* Sections *)
  List.iter
    (fun section ->
      (* Section title *)
      lines := !lines @ [
        Theme.Box.vertical ^ " " ^
        Theme.bold (Theme.pad_right (inner_width - 1) section.title) ^
        Theme.Box.vertical
      ];
      (* Items *)
      List.iter
        (fun (key, desc) ->
          let key_display = Theme.colored Theme.Color.primary (Theme.pad_right 15 key) in
          let item_line = "  " ^ key_display ^ desc in
          lines := !lines @ [
            Theme.Box.vertical ^ " " ^
            Theme.pad_right (inner_width - 1) item_line ^
            Theme.Box.vertical
          ])
        section.items;
      (* Empty line between sections *)
      lines := !lines @ [
        Theme.Box.vertical ^ String.make (width - 2) ' ' ^ Theme.Box.vertical
      ])
    sections;

  (* Footer *)
  lines := !lines @ [
    Theme.Box.vertical ^ " " ^
    Theme.dim (Theme.center (inner_width - 1) "Press ? or Esc to close") ^
    Theme.Box.vertical
  ];

  (* Bottom border *)
  lines := !lines @ [
    Theme.Box.bottom_left ^ String.make (width - 2) '-' ^ Theme.Box.bottom_right
  ];

  (* Pad to height if needed *)
  let current_height = List.length !lines in
  if current_height < height then
    for _ = current_height to height - 1 do
      lines := !lines @ [String.make width ' ']
    done;

  !lines
