(** Input field component *)

(** Input state *)
type t = {
  value : string;
  cursor_pos : int;
  placeholder : string;
  focused : bool;
}

(** Create a new input *)
let create ?(placeholder = "") ?(initial = "") () =
  { value = initial; cursor_pos = String.length initial; placeholder; focused = false }

(** Set focus state *)
let set_focused input focused = { input with focused }

(** Get the current value *)
let value input = input.value

(** Set the value *)
let set_value input value =
  { input with value; cursor_pos = String.length value }

(** Clear the input *)
let clear input = { input with value = ""; cursor_pos = 0 }

(** Handle character input *)
let insert_char input c =
  let before = String.sub input.value 0 input.cursor_pos in
  let after =
    String.sub input.value input.cursor_pos
      (String.length input.value - input.cursor_pos)
  in
  let new_value = before ^ String.make 1 c ^ after in
  { input with value = new_value; cursor_pos = input.cursor_pos + 1 }

(** Handle backspace *)
let backspace input =
  if input.cursor_pos > 0 then
    let before = String.sub input.value 0 (input.cursor_pos - 1) in
    let after =
      String.sub input.value input.cursor_pos
        (String.length input.value - input.cursor_pos)
    in
    { input with value = before ^ after; cursor_pos = input.cursor_pos - 1 }
  else input

(** Handle delete *)
let delete input =
  if input.cursor_pos < String.length input.value then
    let before = String.sub input.value 0 input.cursor_pos in
    let after =
      String.sub input.value (input.cursor_pos + 1)
        (String.length input.value - input.cursor_pos - 1)
    in
    { input with value = before ^ after }
  else input

(** Move cursor left *)
let cursor_left input =
  if input.cursor_pos > 0 then
    { input with cursor_pos = input.cursor_pos - 1 }
  else input

(** Move cursor right *)
let cursor_right input =
  if input.cursor_pos < String.length input.value then
    { input with cursor_pos = input.cursor_pos + 1 }
  else input

(** Move cursor to start *)
let cursor_home input = { input with cursor_pos = 0 }

(** Move cursor to end *)
let cursor_end input = { input with cursor_pos = String.length input.value }

(** Word jump helpers *)
let find_word_boundary_left str pos =
  let rec skip_spaces p =
    if p <= 0 then 0
    else if str.[p - 1] = ' ' then skip_spaces (p - 1)
    else find_word_start p
  and find_word_start p =
    if p <= 0 then 0
    else if str.[p - 1] = ' ' then p
    else find_word_start (p - 1)
  in
  skip_spaces pos

let find_word_boundary_right str pos =
  let len = String.length str in
  let rec skip_word p =
    if p >= len then len
    else if str.[p] = ' ' then skip_spaces p
    else skip_word (p + 1)
  and skip_spaces p =
    if p >= len then len
    else if str.[p] = ' ' then skip_spaces (p + 1)
    else p
  in
  skip_word pos

(** Jump to previous word *)
let cursor_word_left input =
  let new_pos = find_word_boundary_left input.value input.cursor_pos in
  { input with cursor_pos = new_pos }

(** Jump to next word *)
let cursor_word_right input =
  let new_pos = find_word_boundary_right input.value input.cursor_pos in
  { input with cursor_pos = new_pos }

(** Render the input field *)
let render input width =
  let display_value =
    if String.length input.value = 0 && not input.focused then
      Theme.dim input.placeholder
    else if input.focused then
      (* Show cursor *)
      if input.cursor_pos < String.length input.value then
        let before = String.sub input.value 0 input.cursor_pos in
        let cursor = String.sub input.value input.cursor_pos 1 in
        let after =
          String.sub input.value (input.cursor_pos + 1)
            (String.length input.value - input.cursor_pos - 1)
        in
        before ^ Theme.inverse cursor ^ after
      else input.value ^ Theme.inverse " "
    else input.value
  in
  Theme.truncate width display_value
