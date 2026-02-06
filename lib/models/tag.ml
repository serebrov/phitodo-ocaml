(** Tag data model *)

type t = {
  id : string;
  name : string;
  color : string option;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  deleted : bool;
}

(** Create a new tag with defaults *)
let create ~name ?(color = None) () =
  let now = Ptime_clock.now () in
  let id = Uuidm.v4_gen (Random.State.make_self_init ()) () |> Uuidm.to_string in
  { id; name; color; created_at = now; updated_at = now; deleted = false }

(** Mark a tag as deleted (soft delete) *)
let delete tag =
  let now = Ptime_clock.now () in
  { tag with deleted = true; updated_at = now }

(** Update tag's updated_at timestamp *)
let touch tag =
  let now = Ptime_clock.now () in
  { tag with updated_at = now }

(** Format Ptime.t to RFC3339 string *)
let ptime_to_rfc3339 t =
  let (y, m, d), ((hh, mm, ss), _tz_offset) = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" y m d hh mm ss

(** Parse RFC3339 string to Ptime.t option *)
let ptime_of_rfc3339 s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> Some t
  | Error _ -> None
