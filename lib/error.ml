(** Application error types *)

type t =
  | Database of string
  | Io of string
  | Config of string
  | GitHub of string
  | Toggl of string
  | Http of string
  | Json of string
  | Other of string

let to_string = function
  | Database msg -> "Database error: " ^ msg
  | Io msg -> "IO error: " ^ msg
  | Config msg -> "Config error: " ^ msg
  | GitHub msg -> "GitHub error: " ^ msg
  | Toggl msg -> "Toggl error: " ^ msg
  | Http msg -> "HTTP error: " ^ msg
  | Json msg -> "JSON error: " ^ msg
  | Other msg -> "Error: " ^ msg

let database msg = Database msg
let io msg = Io msg
let config msg = Config msg
let github msg = GitHub msg
let toggl msg = Toggl msg
let http msg = Http msg
let json msg = Json msg
let other msg = Other msg

exception App_error of t

let raise_error e = raise (App_error e)

(** Result type alias for convenience *)
type 'a result = ('a, t) Result.t

let ok x = Ok x
let error e = Error e

let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

let map_error f = function
  | Ok x -> Ok x
  | Error e -> Error (f e)

let catch_exn f =
  try Ok (f ())
  with
  | App_error e -> Error e
  | Sqlite3.Error msg -> Error (Database msg)
  | Sys_error msg -> Error (Io msg)
  | Yojson.Json_error msg -> Error (Json msg)
  | exn -> Error (Other (Printexc.to_string exn))
