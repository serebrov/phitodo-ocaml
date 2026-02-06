(** Toggl API service - stub implementation *)

open Toggl_view

(** Toggl data result *)
type toggl_data = {
  entries : time_entry list;
  projects : (int64, string) Hashtbl.t;
}

(** Fetch all Toggl data - stub that returns empty data *)
let fetch_all ~token:_ ~days:_ =
  Ok { entries = []; projects = Hashtbl.create 0 }
