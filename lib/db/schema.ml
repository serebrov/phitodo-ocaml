(** Database schema definitions and initialization *)

let schema_version = 1

let create_schema_version_table =
  {|
  CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY
  )
|}

let create_tasks_table =
  {|
  CREATE TABLE IF NOT EXISTS tasks (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    notes TEXT,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    due_date TEXT,
    start_date TEXT,
    completed_at TEXT,
    project_id TEXT,
    priority TEXT NOT NULL DEFAULT 'none',
    status TEXT NOT NULL DEFAULT 'inbox',
    order_index INTEGER NOT NULL DEFAULT 0,
    deleted INTEGER NOT NULL DEFAULT 0,
    kind TEXT,
    size TEXT,
    assignee TEXT,
    context_url TEXT,
    metadata TEXT
  )
|}

let create_projects_table =
  {|
  CREATE TABLE IF NOT EXISTS projects (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT,
    color TEXT,
    icon TEXT,
    order_index INTEGER NOT NULL DEFAULT 0,
    is_inbox INTEGER NOT NULL DEFAULT 0,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    deleted INTEGER NOT NULL DEFAULT 0
  )
|}

let create_tags_table =
  {|
  CREATE TABLE IF NOT EXISTS tags (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    color TEXT,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    deleted INTEGER NOT NULL DEFAULT 0
  )
|}

let create_task_tags_table =
  {|
  CREATE TABLE IF NOT EXISTS task_tags (
    task_id TEXT NOT NULL,
    tag_id TEXT NOT NULL,
    PRIMARY KEY (task_id, tag_id),
    FOREIGN KEY (task_id) REFERENCES tasks(id) ON DELETE CASCADE,
    FOREIGN KEY (tag_id) REFERENCES tags(id) ON DELETE CASCADE
  )
|}

let create_indexes =
  [
    "CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status)";
    "CREATE INDEX IF NOT EXISTS idx_tasks_due_date ON tasks(due_date)";
    "CREATE INDEX IF NOT EXISTS idx_tasks_project ON tasks(project_id)";
    "CREATE INDEX IF NOT EXISTS idx_task_tags_task ON task_tags(task_id)";
    "CREATE INDEX IF NOT EXISTS idx_task_tags_tag ON task_tags(tag_id)";
  ]

let insert_default_inbox =
  {|
  INSERT OR IGNORE INTO projects (id, name, description, color, icon, order_index, is_inbox, created_at, updated_at, deleted)
  VALUES ('inbox', 'Inbox', 'Default inbox for new tasks', '#4A90D9', '📥', 0, 1, datetime('now'), datetime('now'), 0)
|}

(** Initialize the database schema *)
let init db =
  let open Sqlite3 in
  let exec_sql sql =
    match exec db sql with
    | Rc.OK -> ()
    | rc -> failwith (Printf.sprintf "SQL error: %s - %s" (Rc.to_string rc) (errmsg db))
  in
  (* Create tables *)
  exec_sql create_schema_version_table;
  exec_sql create_tasks_table;
  exec_sql create_projects_table;
  exec_sql create_tags_table;
  exec_sql create_task_tags_table;
  (* Create indexes *)
  List.iter exec_sql create_indexes;
  (* Insert default inbox project *)
  exec_sql insert_default_inbox;
  (* Set schema version *)
  let set_version =
    Printf.sprintf
      "INSERT OR REPLACE INTO schema_version (version) VALUES (%d)"
      schema_version
  in
  exec_sql set_version

(** Get the current schema version *)
let get_version db =
  let open Sqlite3 in
  let stmt = prepare db "SELECT version FROM schema_version LIMIT 1" in
  match step stmt with
  | Rc.ROW ->
      let version = column_int stmt 0 in
      ignore (finalize stmt);
      Some version
  | _ ->
      ignore (finalize stmt);
      None

(** Check if database needs initialization *)
let needs_init db =
  match get_version db with
  | None -> true
  | Some v -> v < schema_version
