(** Modal dialog components *)

(** Confirm modal state *)
type confirm_t = {
  title : string;
  message : string;
  confirm_label : string;
  cancel_label : string;
  selected_confirm : bool;
}

(** Create a confirm modal *)
let create_confirm ~title ~message ?(confirm_label = "Confirm")
    ?(cancel_label = "Cancel") () =
  { title; message; confirm_label; cancel_label; selected_confirm = false }

(** Toggle selection in confirm modal *)
let toggle_confirm modal =
  { modal with selected_confirm = not modal.selected_confirm }

(** Select confirm in confirm modal *)
let select_confirm modal = { modal with selected_confirm = true }

(** Select cancel in confirm modal *)
let select_cancel modal = { modal with selected_confirm = false }

(** Render confirm modal *)
let render_confirm modal width =
  let lines = ref [] in
  let inner_width = width - 4 in

  (* Top border *)
  lines := !lines @ [
    Theme.Box.top_left ^ String.make (width - 2) '-' ^ Theme.Box.top_right
  ];

  (* Title *)
  lines := !lines @ [
    Theme.Box.vertical ^ " " ^
    Theme.bold (Theme.pad_right (inner_width - 1) modal.title) ^
    Theme.Box.vertical
  ];

  (* Separator *)
  lines := !lines @ [
    Theme.Box.t_right ^ String.make (width - 2) '-' ^ Theme.Box.t_left
  ];

  (* Message *)
  lines := !lines @ [
    Theme.Box.vertical ^ " " ^
    Theme.pad_right (inner_width - 1) modal.message ^
    Theme.Box.vertical
  ];

  (* Empty line *)
  lines := !lines @ [
    Theme.Box.vertical ^ String.make (width - 2) ' ' ^ Theme.Box.vertical
  ];

  (* Buttons *)
  let confirm_btn =
    if modal.selected_confirm then
      Theme.inverse (" " ^ modal.confirm_label ^ " ")
    else " " ^ modal.confirm_label ^ " "
  in
  let cancel_btn =
    if not modal.selected_confirm then
      Theme.inverse (" " ^ modal.cancel_label ^ " ")
    else " " ^ modal.cancel_label ^ " "
  in
  let buttons = cancel_btn ^ "    " ^ confirm_btn in
  let buttons_padded = Theme.center (inner_width - 1) buttons in
  lines := !lines @ [
    Theme.Box.vertical ^ " " ^ buttons_padded ^ Theme.Box.vertical
  ];

  (* Bottom border *)
  lines := !lines @ [
    Theme.Box.bottom_left ^ String.make (width - 2) '-' ^ Theme.Box.bottom_right
  ];

  !lines

(** Input modal state *)
type input_t = {
  title : string;
  prompt : string;
  value : string;
  cursor_pos : int;
}

(** Create an input modal *)
let create_input ~title ~prompt ?(initial = "") () =
  { title; prompt; value = initial; cursor_pos = String.length initial }

(** Handle character input *)
let input_char modal c =
  let before = String.sub modal.value 0 modal.cursor_pos in
  let after =
    String.sub modal.value modal.cursor_pos
      (String.length modal.value - modal.cursor_pos)
  in
  let new_value = before ^ String.make 1 c ^ after in
  { modal with value = new_value; cursor_pos = modal.cursor_pos + 1 }

(** Handle backspace *)
let input_backspace modal =
  if modal.cursor_pos > 0 then
    let before = String.sub modal.value 0 (modal.cursor_pos - 1) in
    let after =
      String.sub modal.value modal.cursor_pos
        (String.length modal.value - modal.cursor_pos)
    in
    { modal with value = before ^ after; cursor_pos = modal.cursor_pos - 1 }
  else modal

(** Handle delete *)
let input_delete modal =
  if modal.cursor_pos < String.length modal.value then
    let before = String.sub modal.value 0 modal.cursor_pos in
    let after =
      String.sub modal.value (modal.cursor_pos + 1)
        (String.length modal.value - modal.cursor_pos - 1)
    in
    { modal with value = before ^ after }
  else modal

(** Move cursor left *)
let input_left modal =
  if modal.cursor_pos > 0 then
    { modal with cursor_pos = modal.cursor_pos - 1 }
  else modal

(** Move cursor right *)
let input_right modal =
  if modal.cursor_pos < String.length modal.value then
    { modal with cursor_pos = modal.cursor_pos + 1 }
  else modal

(** Move cursor to start *)
let input_home modal = { modal with cursor_pos = 0 }

(** Move cursor to end *)
let input_end modal = { modal with cursor_pos = String.length modal.value }

(** Render input modal *)
let render_input modal width =
  let lines = ref [] in
  let inner_width = width - 4 in

  (* Top border *)
  lines := !lines @ [
    Theme.Box.top_left ^ String.make (width - 2) '-' ^ Theme.Box.top_right
  ];

  (* Title *)
  lines := !lines @ [
    Theme.Box.vertical ^ " " ^
    Theme.bold (Theme.pad_right (inner_width - 1) modal.title) ^
    Theme.Box.vertical
  ];

  (* Separator *)
  lines := !lines @ [
    Theme.Box.t_right ^ String.make (width - 2) '-' ^ Theme.Box.t_left
  ];

  (* Prompt *)
  lines := !lines @ [
    Theme.Box.vertical ^ " " ^
    Theme.pad_right (inner_width - 1) modal.prompt ^
    Theme.Box.vertical
  ];

  (* Input field with cursor *)
  let value_display =
    if modal.cursor_pos < String.length modal.value then
      let before = String.sub modal.value 0 modal.cursor_pos in
      let cursor = String.sub modal.value modal.cursor_pos 1 in
      let after =
        String.sub modal.value (modal.cursor_pos + 1)
          (String.length modal.value - modal.cursor_pos - 1)
      in
      before ^ Theme.inverse cursor ^ after
    else modal.value ^ Theme.inverse " "
  in
  let input_line = "> " ^ value_display in
  lines := !lines @ [
    Theme.Box.vertical ^ " " ^
    Theme.pad_right (inner_width - 1) input_line ^
    Theme.Box.vertical
  ];

  (* Help text *)
  lines := !lines @ [
    Theme.Box.vertical ^ " " ^
    Theme.dim (Theme.pad_right (inner_width - 1) "Enter: Confirm  Esc: Cancel") ^
    Theme.Box.vertical
  ];

  (* Bottom border *)
  lines := !lines @ [
    Theme.Box.bottom_left ^ String.make (width - 2) '-' ^ Theme.Box.bottom_right
  ];

  !lines

(** Notification modal state *)
type notification_t = {
  message : string;
  is_error : bool;
}

(** Create a notification *)
let create_notification ?(is_error = false) message =
  { message; is_error }

(** Render notification *)
let render_notification notif width =
  let color = if notif.is_error then Theme.Color.error else Theme.Color.success in
  let content = Theme.colored color notif.message in
  [Theme.center width content]
