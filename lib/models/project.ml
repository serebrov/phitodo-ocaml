(** Project data model *)

type t = {
  id : string;
  name : string;
  description : string option;
  color : string option;
  icon : string option;
  order_index : int64;
  is_inbox : bool;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  deleted : bool;
}

(** Create a new project with defaults *)
let create ~name ?(description = None) ?(color = None) ?(icon = None)
    ?(order_index = 0L) ?(is_inbox = false) () =
  let now = Ptime_clock.now () in
  let id = Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string in
  {
    id;
    name;
    description;
    color;
    icon;
    order_index;
    is_inbox;
    created_at = now;
    updated_at = now;
    deleted = false;
  }

(** Create the special inbox project *)
let create_inbox () =
  let now = Ptime_clock.now () in
  {
    id = "inbox";
    name = "Inbox";
    description = Some "Default inbox for new tasks";
    color = Some "#4A90D9";
    icon = Some "📥";
    order_index = 0L;
    is_inbox = true;
    created_at = now;
    updated_at = now;
    deleted = false;
  }

(** Mark a project as deleted (soft delete) *)
let delete project =
  let now = Ptime_clock.now () in
  { project with deleted = true; updated_at = now }

(** Update project's updated_at timestamp *)
let touch project =
  let now = Ptime_clock.now () in
  { project with updated_at = now }

(** Format Ptime.t to RFC3339 string *)
let ptime_to_rfc3339 t =
  let (y, m, d), ((hh, mm, ss), _tz_offset) = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" y m d hh mm ss

(** Parse RFC3339 string to Ptime.t option *)
let ptime_of_rfc3339 s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> Some t
  | Error _ -> None
