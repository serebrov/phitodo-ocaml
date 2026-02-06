(** Database repository for CRUD operations *)

(** Helper to get optional text from column *)
let column_text_opt stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.NULL -> None
  | Sqlite3.Data.TEXT s -> Some s
  | _ -> None

(** Helper to get text from column with default *)
let column_text_default stmt i default =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | _ -> default

(** Helper to get int64 from column *)
let column_int64_default stmt i default =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT i -> i
  | _ -> default

(** Helper to get int from column *)
let column_int_default stmt i default =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT i -> Int64.to_int i
  | _ -> default

(** Helper to bind NULL to a column *)
let bind_null stmt i =
  Sqlite3.bind stmt i Sqlite3.Data.NULL

module Task = struct
  open Task

  (** Convert metadata list to JSON string *)
  let metadata_to_json metadata =
    let assoc = List.map (fun (k, v) -> (k, `String v)) metadata in
    Yojson.Safe.to_string (`Assoc assoc)

  (** Convert JSON string to metadata list *)
  let metadata_of_json s =
    try
      match Yojson.Safe.from_string s with
      | `Assoc assoc ->
          List.filter_map
            (fun (k, v) ->
              match v with
              | `String s -> Some (k, s)
              | _ -> None)
            assoc
      | _ -> []
    with _ -> []

  (** Get tags for a specific task *)
  let get_task_tags db task_id =
    let open Sqlite3 in
    let sql = "SELECT tag_id FROM task_tags WHERE task_id = ?" in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 task_id);
    let tags = ref [] in
    while step stmt = Rc.ROW do
      tags := column_text_default stmt 0 "" :: !tags
    done;
    ignore (finalize stmt);
    List.rev !tags

  (** Get all tasks from the database *)
  let get_all db =
    let open Sqlite3 in
    let sql =
      {|
      SELECT id, title, notes, created_at, updated_at, due_date, start_date,
             completed_at, project_id, priority, status, order_index, deleted,
             kind, size, assignee, context_url, metadata
      FROM tasks
      WHERE deleted = 0
      ORDER BY order_index ASC, created_at DESC
    |}
    in
    let stmt = prepare db sql in
    let tasks = ref [] in
    while step stmt = Rc.ROW do
      let id = column_text_default stmt 0 "" in
      let title = column_text_default stmt 1 "" in
      let notes = column_text_opt stmt 2 in
      let created_at =
        match column_text_opt stmt 3 with
        | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
        | None -> Ptime_clock.now ()
      in
      let updated_at =
        match column_text_opt stmt 4 with
        | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
        | None -> Ptime_clock.now ()
      in
      let due_date =
        match column_text_opt stmt 5 with
        | Some s -> parse_date_string s
        | None -> None
      in
      let start_date =
        match column_text_opt stmt 6 with
        | Some s -> parse_date_string s
        | None -> None
      in
      let completed_at =
        match column_text_opt stmt 7 with
        | Some s -> ptime_of_rfc3339 s
        | None -> None
      in
      let project_id = column_text_opt stmt 8 in
      let priority = task_priority_of_string (column_text_default stmt 9 "none") in
      let status = task_status_of_string (column_text_default stmt 10 "inbox") in
      let order_index = column_int64_default stmt 11 0L in
      let deleted = column_int_default stmt 12 0 <> 0 in
      let kind =
        match column_text_opt stmt 13 with
        | Some s -> Some (task_kind_of_string s)
        | None -> None
      in
      let size =
        match column_text_opt stmt 14 with
        | Some s -> Some (task_size_of_string s)
        | None -> None
      in
      let assignee = column_text_opt stmt 15 in
      let context_url = column_text_opt stmt 16 in
      let metadata =
        match column_text_opt stmt 17 with
        | Some s -> metadata_of_json s
        | None -> []
      in
      tasks :=
        {
          id;
          title;
          notes;
          created_at;
          updated_at;
          due_date;
          start_date;
          completed_at;
          project_id;
          priority;
          tags = [];
          status;
          order_index;
          deleted;
          kind;
          size;
          assignee;
          context_url;
          metadata;
        }
        :: !tasks
    done;
    ignore (finalize stmt);
    (* Load tags for each task *)
    List.rev_map
      (fun task ->
        let tags = get_task_tags db task.id in
        { task with tags })
      !tasks

  (** Get a single task by ID *)
  let get db id =
    let open Sqlite3 in
    let sql =
      {|
      SELECT id, title, notes, created_at, updated_at, due_date, start_date,
             completed_at, project_id, priority, status, order_index, deleted,
             kind, size, assignee, context_url, metadata
      FROM tasks
      WHERE id = ?
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 id);
    let result =
      if step stmt = Rc.ROW then (
        let id = column_text_default stmt 0 "" in
        let title = column_text_default stmt 1 "" in
        let notes = column_text_opt stmt 2 in
        let created_at =
          match column_text_opt stmt 3 with
          | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
          | None -> Ptime_clock.now ()
        in
        let updated_at =
          match column_text_opt stmt 4 with
          | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
          | None -> Ptime_clock.now ()
        in
        let due_date =
          match column_text_opt stmt 5 with
          | Some s -> parse_date_string s
          | None -> None
        in
        let start_date =
          match column_text_opt stmt 6 with
          | Some s -> parse_date_string s
          | None -> None
        in
        let completed_at =
          match column_text_opt stmt 7 with
          | Some s -> ptime_of_rfc3339 s
          | None -> None
        in
        let project_id = column_text_opt stmt 8 in
        let priority = task_priority_of_string (column_text_default stmt 9 "none") in
        let status = task_status_of_string (column_text_default stmt 10 "inbox") in
        let order_index = column_int64_default stmt 11 0L in
        let deleted = column_int_default stmt 12 0 <> 0 in
        let kind =
          match column_text_opt stmt 13 with
          | Some s -> Some (task_kind_of_string s)
          | None -> None
        in
        let size =
          match column_text_opt stmt 14 with
          | Some s -> Some (task_size_of_string s)
          | None -> None
        in
        let assignee = column_text_opt stmt 15 in
        let context_url = column_text_opt stmt 16 in
        let metadata =
          match column_text_opt stmt 17 with
          | Some s -> metadata_of_json s
          | None -> []
        in
        let tags = get_task_tags db id in
        Some
          {
            id;
            title;
            notes;
            created_at;
            updated_at;
            due_date;
            start_date;
            completed_at;
            project_id;
            priority;
            tags;
            status;
            order_index;
            deleted;
            kind;
            size;
            assignee;
            context_url;
            metadata;
          })
      else None
    in
    ignore (finalize stmt);
    result

  (** Insert a new task *)
  let insert db (task : Task.t) =
    let open Sqlite3 in
    let sql =
      {|
      INSERT INTO tasks (id, title, notes, created_at, updated_at, due_date, start_date,
                         completed_at, project_id, priority, status, order_index, deleted,
                         kind, size, assignee, context_url, metadata)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 task.id);
    ignore (bind_text stmt 2 task.title);
    (match task.notes with
    | Some n -> ignore (bind_text stmt 3 n)
    | None -> ignore (bind_null stmt 3));
    ignore (bind_text stmt 4 (ptime_to_rfc3339 task.created_at));
    ignore (bind_text stmt 5 (ptime_to_rfc3339 task.updated_at));
    (match task.due_date with
    | Some (y, m, d) ->
        ignore (bind_text stmt 6 (Printf.sprintf "%04d-%02d-%02d" y m d))
    | None -> ignore (bind_null stmt 6));
    (match task.start_date with
    | Some (y, m, d) ->
        ignore (bind_text stmt 7 (Printf.sprintf "%04d-%02d-%02d" y m d))
    | None -> ignore (bind_null stmt 7));
    (match task.completed_at with
    | Some t -> ignore (bind_text stmt 8 (ptime_to_rfc3339 t))
    | None -> ignore (bind_null stmt 8));
    (match task.project_id with
    | Some p -> ignore (bind_text stmt 9 p)
    | None -> ignore (bind_null stmt 9));
    ignore (bind_text stmt 10 (task_priority_to_string task.priority));
    ignore (bind_text stmt 11 (task_status_to_string task.status));
    ignore (bind_int64 stmt 12 task.order_index);
    ignore (bind_int stmt 13 (if task.deleted then 1 else 0));
    (match task.kind with
    | Some k -> ignore (bind_text stmt 14 (task_kind_to_string k))
    | None -> ignore (bind_null stmt 14));
    (match task.size with
    | Some s -> ignore (bind_text stmt 15 (task_size_to_string s))
    | None -> ignore (bind_null stmt 15));
    (match task.assignee with
    | Some a -> ignore (bind_text stmt 16 a)
    | None -> ignore (bind_null stmt 16));
    (match task.context_url with
    | Some u -> ignore (bind_text stmt 17 u)
    | None -> ignore (bind_null stmt 17));
    ignore (bind_text stmt 18 (metadata_to_json task.metadata));
    let rc = step stmt in
    ignore (finalize stmt);
    if rc <> Rc.DONE then
      failwith (Printf.sprintf "Failed to insert task: %s" (errmsg db));
    (* Insert tags *)
    List.iter
      (fun tag_id ->
        let tag_sql =
          "INSERT OR IGNORE INTO task_tags (task_id, tag_id) VALUES (?, ?)"
        in
        let tag_stmt = prepare db tag_sql in
        ignore (bind_text tag_stmt 1 task.id);
        ignore (bind_text tag_stmt 2 tag_id);
        ignore (step tag_stmt);
        ignore (finalize tag_stmt))
      task.tags

  (** Update an existing task *)
  let update db (task : Task.t) =
    let open Sqlite3 in
    let sql =
      {|
      UPDATE tasks SET
        title = ?, notes = ?, updated_at = ?, due_date = ?, start_date = ?,
        completed_at = ?, project_id = ?, priority = ?, status = ?, order_index = ?,
        deleted = ?, kind = ?, size = ?, assignee = ?, context_url = ?, metadata = ?
      WHERE id = ?
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 task.title);
    (match task.notes with
    | Some n -> ignore (bind_text stmt 2 n)
    | None -> ignore (bind_null stmt 2));
    ignore (bind_text stmt 3 (ptime_to_rfc3339 task.updated_at));
    (match task.due_date with
    | Some (y, m, d) ->
        ignore (bind_text stmt 4 (Printf.sprintf "%04d-%02d-%02d" y m d))
    | None -> ignore (bind_null stmt 4));
    (match task.start_date with
    | Some (y, m, d) ->
        ignore (bind_text stmt 5 (Printf.sprintf "%04d-%02d-%02d" y m d))
    | None -> ignore (bind_null stmt 5));
    (match task.completed_at with
    | Some t -> ignore (bind_text stmt 6 (ptime_to_rfc3339 t))
    | None -> ignore (bind_null stmt 6));
    (match task.project_id with
    | Some p -> ignore (bind_text stmt 7 p)
    | None -> ignore (bind_null stmt 7));
    ignore (bind_text stmt 8 (task_priority_to_string task.priority));
    ignore (bind_text stmt 9 (task_status_to_string task.status));
    ignore (bind_int64 stmt 10 task.order_index);
    ignore (bind_int stmt 11 (if task.deleted then 1 else 0));
    (match task.kind with
    | Some k -> ignore (bind_text stmt 12 (task_kind_to_string k))
    | None -> ignore (bind_null stmt 12));
    (match task.size with
    | Some s -> ignore (bind_text stmt 13 (task_size_to_string s))
    | None -> ignore (bind_null stmt 13));
    (match task.assignee with
    | Some a -> ignore (bind_text stmt 14 a)
    | None -> ignore (bind_null stmt 14));
    (match task.context_url with
    | Some u -> ignore (bind_text stmt 15 u)
    | None -> ignore (bind_null stmt 15));
    ignore (bind_text stmt 16 (metadata_to_json task.metadata));
    ignore (bind_text stmt 17 task.id);
    let rc = step stmt in
    ignore (finalize stmt);
    if rc <> Rc.DONE then
      failwith (Printf.sprintf "Failed to update task: %s" (errmsg db));
    (* Update tags: delete all and re-insert *)
    let del_sql = "DELETE FROM task_tags WHERE task_id = ?" in
    let del_stmt = prepare db del_sql in
    ignore (bind_text del_stmt 1 task.id);
    ignore (step del_stmt);
    ignore (finalize del_stmt);
    List.iter
      (fun tag_id ->
        let tag_sql =
          "INSERT OR IGNORE INTO task_tags (task_id, tag_id) VALUES (?, ?)"
        in
        let tag_stmt = prepare db tag_sql in
        ignore (bind_text tag_stmt 1 task.id);
        ignore (bind_text tag_stmt 2 tag_id);
        ignore (step tag_stmt);
        ignore (finalize tag_stmt))
      task.tags

  (** Soft delete a task *)
  let delete db id =
    let open Sqlite3 in
    let sql =
      "UPDATE tasks SET deleted = 1, updated_at = ? WHERE id = ?"
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 (ptime_to_rfc3339 (Ptime_clock.now ())));
    ignore (bind_text stmt 2 id);
    let rc = step stmt in
    ignore (finalize stmt);
    if rc <> Rc.DONE then
      failwith (Printf.sprintf "Failed to delete task: %s" (errmsg db))

  (** Count tasks by status *)
  let count_by_status db status =
    let open Sqlite3 in
    let sql =
      "SELECT COUNT(*) FROM tasks WHERE status = ? AND deleted = 0"
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 (task_status_to_string status));
    let count =
      if step stmt = Rc.ROW then column_int64_default stmt 0 0L else 0L
    in
    ignore (finalize stmt);
    count

  (** Count tasks due today *)
  let count_due_today db =
    let open Sqlite3 in
    let today =
      let y, m, d = Ptime.to_date (Ptime_clock.now ()) in
      Printf.sprintf "%04d-%02d-%02d" y m d
    in
    let sql =
      {|
      SELECT COUNT(*) FROM tasks
      WHERE due_date <= ? AND status != 'completed' AND deleted = 0
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 today);
    let count =
      if step stmt = Rc.ROW then column_int64_default stmt 0 0L else 0L
    in
    ignore (finalize stmt);
    count

  (** Count overdue tasks *)
  let count_overdue db =
    let open Sqlite3 in
    let today =
      let y, m, d = Ptime.to_date (Ptime_clock.now ()) in
      Printf.sprintf "%04d-%02d-%02d" y m d
    in
    let sql =
      {|
      SELECT COUNT(*) FROM tasks
      WHERE due_date < ? AND status != 'completed' AND deleted = 0
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 today);
    let count =
      if step stmt = Rc.ROW then column_int64_default stmt 0 0L else 0L
    in
    ignore (finalize stmt);
    count

  (** Count tasks for a specific project *)
  let count_for_project db project_id =
    let open Sqlite3 in
    let sql =
      {|
      SELECT COUNT(*) FROM tasks
      WHERE project_id = ? AND status != 'completed' AND deleted = 0
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 project_id);
    let count =
      if step stmt = Rc.ROW then column_int64_default stmt 0 0L else 0L
    in
    ignore (finalize stmt);
    count

  (** Get next order index *)
  let get_next_order_index db =
    let open Sqlite3 in
    let sql = "SELECT COALESCE(MAX(order_index), 0) + 1 FROM tasks" in
    let stmt = prepare db sql in
    let index =
      if step stmt = Rc.ROW then column_int64_default stmt 0 1L else 1L
    in
    ignore (finalize stmt);
    index
end

module Project = struct
  open Project

  (** Get all projects from the database *)
  let get_all db =
    let open Sqlite3 in
    let sql =
      {|
      SELECT id, name, description, color, icon, order_index, is_inbox,
             created_at, updated_at, deleted
      FROM projects
      WHERE deleted = 0
      ORDER BY order_index ASC, name ASC
    |}
    in
    let stmt = prepare db sql in
    let projects = ref [] in
    while step stmt = Rc.ROW do
      let id = column_text_default stmt 0 "" in
      let name = column_text_default stmt 1 "" in
      let description = column_text_opt stmt 2 in
      let color = column_text_opt stmt 3 in
      let icon = column_text_opt stmt 4 in
      let order_index = column_int64_default stmt 5 0L in
      let is_inbox = column_int_default stmt 6 0 <> 0 in
      let created_at =
        match column_text_opt stmt 7 with
        | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
        | None -> Ptime_clock.now ()
      in
      let updated_at =
        match column_text_opt stmt 8 with
        | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
        | None -> Ptime_clock.now ()
      in
      let deleted = column_int_default stmt 9 0 <> 0 in
      projects :=
        {
          id;
          name;
          description;
          color;
          icon;
          order_index;
          is_inbox;
          created_at;
          updated_at;
          deleted;
        }
        :: !projects
    done;
    ignore (finalize stmt);
    List.rev !projects

  (** Get a single project by ID *)
  let get db id =
    let open Sqlite3 in
    let sql =
      {|
      SELECT id, name, description, color, icon, order_index, is_inbox,
             created_at, updated_at, deleted
      FROM projects
      WHERE id = ?
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 id);
    let result =
      if step stmt = Rc.ROW then (
        let id = column_text_default stmt 0 "" in
        let name = column_text_default stmt 1 "" in
        let description = column_text_opt stmt 2 in
        let color = column_text_opt stmt 3 in
        let icon = column_text_opt stmt 4 in
        let order_index = column_int64_default stmt 5 0L in
        let is_inbox = column_int_default stmt 6 0 <> 0 in
        let created_at =
          match column_text_opt stmt 7 with
          | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
          | None -> Ptime_clock.now ()
        in
        let updated_at =
          match column_text_opt stmt 8 with
          | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
          | None -> Ptime_clock.now ()
        in
        let deleted = column_int_default stmt 9 0 <> 0 in
        Some
          {
            id;
            name;
            description;
            color;
            icon;
            order_index;
            is_inbox;
            created_at;
            updated_at;
            deleted;
          })
      else None
    in
    ignore (finalize stmt);
    result

  (** Insert a new project *)
  let insert db (project : Project.t) =
    let open Sqlite3 in
    let sql =
      {|
      INSERT INTO projects (id, name, description, color, icon, order_index,
                            is_inbox, created_at, updated_at, deleted)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 project.id);
    ignore (bind_text stmt 2 project.name);
    (match project.description with
    | Some d -> ignore (bind_text stmt 3 d)
    | None -> ignore (bind_null stmt 3));
    (match project.color with
    | Some c -> ignore (bind_text stmt 4 c)
    | None -> ignore (bind_null stmt 4));
    (match project.icon with
    | Some i -> ignore (bind_text stmt 5 i)
    | None -> ignore (bind_null stmt 5));
    ignore (bind_int64 stmt 6 project.order_index);
    ignore (bind_int stmt 7 (if project.is_inbox then 1 else 0));
    ignore (bind_text stmt 8 (ptime_to_rfc3339 project.created_at));
    ignore (bind_text stmt 9 (ptime_to_rfc3339 project.updated_at));
    ignore (bind_int stmt 10 (if project.deleted then 1 else 0));
    let rc = step stmt in
    ignore (finalize stmt);
    if rc <> Rc.DONE then
      failwith (Printf.sprintf "Failed to insert project: %s" (errmsg db))

  (** Update an existing project *)
  let update db (project : Project.t) =
    let open Sqlite3 in
    let sql =
      {|
      UPDATE projects SET
        name = ?, description = ?, color = ?, icon = ?, order_index = ?,
        is_inbox = ?, updated_at = ?, deleted = ?
      WHERE id = ?
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 project.name);
    (match project.description with
    | Some d -> ignore (bind_text stmt 2 d)
    | None -> ignore (bind_null stmt 2));
    (match project.color with
    | Some c -> ignore (bind_text stmt 3 c)
    | None -> ignore (bind_null stmt 3));
    (match project.icon with
    | Some i -> ignore (bind_text stmt 4 i)
    | None -> ignore (bind_null stmt 4));
    ignore (bind_int64 stmt 5 project.order_index);
    ignore (bind_int stmt 6 (if project.is_inbox then 1 else 0));
    ignore (bind_text stmt 7 (ptime_to_rfc3339 project.updated_at));
    ignore (bind_int stmt 8 (if project.deleted then 1 else 0));
    ignore (bind_text stmt 9 project.id);
    let rc = step stmt in
    ignore (finalize stmt);
    if rc <> Rc.DONE then
      failwith (Printf.sprintf "Failed to update project: %s" (errmsg db))

  (** Soft delete a project *)
  let delete db id =
    let open Sqlite3 in
    let sql =
      "UPDATE projects SET deleted = 1, updated_at = ? WHERE id = ?"
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 (ptime_to_rfc3339 (Ptime_clock.now ())));
    ignore (bind_text stmt 2 id);
    let rc = step stmt in
    ignore (finalize stmt);
    if rc <> Rc.DONE then
      failwith (Printf.sprintf "Failed to delete project: %s" (errmsg db))

  (** Get next order index *)
  let get_next_order_index db =
    let open Sqlite3 in
    let sql = "SELECT COALESCE(MAX(order_index), 0) + 1 FROM projects" in
    let stmt = prepare db sql in
    let index =
      if step stmt = Rc.ROW then column_int64_default stmt 0 1L else 1L
    in
    ignore (finalize stmt);
    index
end

module Tag = struct
  open Tag

  (** Get all tags from the database *)
  let get_all db =
    let open Sqlite3 in
    let sql =
      {|
      SELECT id, name, color, created_at, updated_at, deleted
      FROM tags
      WHERE deleted = 0
      ORDER BY name ASC
    |}
    in
    let stmt = prepare db sql in
    let tags = ref [] in
    while step stmt = Rc.ROW do
      let id = column_text_default stmt 0 "" in
      let name = column_text_default stmt 1 "" in
      let color = column_text_opt stmt 2 in
      let created_at =
        match column_text_opt stmt 3 with
        | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
        | None -> Ptime_clock.now ()
      in
      let updated_at =
        match column_text_opt stmt 4 with
        | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
        | None -> Ptime_clock.now ()
      in
      let deleted = column_int_default stmt 5 0 <> 0 in
      tags :=
        { id; name; color; created_at; updated_at; deleted } :: !tags
    done;
    ignore (finalize stmt);
    List.rev !tags

  (** Get a single tag by ID *)
  let get db id =
    let open Sqlite3 in
    let sql =
      {|
      SELECT id, name, color, created_at, updated_at, deleted
      FROM tags
      WHERE id = ?
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 id);
    let result =
      if step stmt = Rc.ROW then (
        let id = column_text_default stmt 0 "" in
        let name = column_text_default stmt 1 "" in
        let color = column_text_opt stmt 2 in
        let created_at =
          match column_text_opt stmt 3 with
          | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
          | None -> Ptime_clock.now ()
        in
        let updated_at =
          match column_text_opt stmt 4 with
          | Some s -> (match ptime_of_rfc3339 s with Some t -> t | None -> Ptime_clock.now ())
          | None -> Ptime_clock.now ()
        in
        let deleted = column_int_default stmt 5 0 <> 0 in
        Some { id; name; color; created_at; updated_at; deleted })
      else None
    in
    ignore (finalize stmt);
    result

  (** Insert a new tag *)
  let insert db (tag : Tag.t) =
    let open Sqlite3 in
    let sql =
      {|
      INSERT INTO tags (id, name, color, created_at, updated_at, deleted)
      VALUES (?, ?, ?, ?, ?, ?)
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 tag.id);
    ignore (bind_text stmt 2 tag.name);
    (match tag.color with
    | Some c -> ignore (bind_text stmt 3 c)
    | None -> ignore (bind_null stmt 3));
    ignore (bind_text stmt 4 (ptime_to_rfc3339 tag.created_at));
    ignore (bind_text stmt 5 (ptime_to_rfc3339 tag.updated_at));
    ignore (bind_int stmt 6 (if tag.deleted then 1 else 0));
    let rc = step stmt in
    ignore (finalize stmt);
    if rc <> Rc.DONE then
      failwith (Printf.sprintf "Failed to insert tag: %s" (errmsg db))

  (** Update an existing tag *)
  let update db (tag : Tag.t) =
    let open Sqlite3 in
    let sql =
      {|
      UPDATE tags SET
        name = ?, color = ?, updated_at = ?, deleted = ?
      WHERE id = ?
    |}
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 tag.name);
    (match tag.color with
    | Some c -> ignore (bind_text stmt 2 c)
    | None -> ignore (bind_null stmt 2));
    ignore (bind_text stmt 3 (ptime_to_rfc3339 tag.updated_at));
    ignore (bind_int stmt 4 (if tag.deleted then 1 else 0));
    ignore (bind_text stmt 5 tag.id);
    let rc = step stmt in
    ignore (finalize stmt);
    if rc <> Rc.DONE then
      failwith (Printf.sprintf "Failed to update tag: %s" (errmsg db))

  (** Soft delete a tag *)
  let delete db id =
    let open Sqlite3 in
    let sql =
      "UPDATE tags SET deleted = 1, updated_at = ? WHERE id = ?"
    in
    let stmt = prepare db sql in
    ignore (bind_text stmt 1 (ptime_to_rfc3339 (Ptime_clock.now ())));
    ignore (bind_text stmt 2 id);
    let rc = step stmt in
    ignore (finalize stmt);
    if rc <> Rc.DONE then
      failwith (Printf.sprintf "Failed to delete tag: %s" (errmsg db))
end

(** Open or create the database *)
let open_db path =
  let db = Sqlite3.db_open path in
  if Schema.needs_init db then Schema.init db;
  db

(** Close the database *)
let close_db db =
  ignore (Sqlite3.db_close db)
