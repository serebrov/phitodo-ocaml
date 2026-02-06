(** Task detail panel component *)

(** Render the task detail panel *)
let render task_opt width =
  let lines = ref [] in

  (* Title *)
  lines := !lines @ [Theme.bold (Theme.colored Theme.Color.primary "Details")];
  lines := !lines @ [String.make width '-'];

  match task_opt with
  | None ->
      lines := !lines @ [Theme.dim (Theme.center width "No task selected")];
      !lines
  | Some (task : Task.t) ->
      (* Task title *)
      lines := !lines @ [Theme.bold task.title];
      lines := !lines @ [""];

      (* Status and priority *)
      let status_line =
        Printf.sprintf "Status: %s  Priority: %s"
          (Theme.format_status task.status)
          (Theme.format_priority task.priority)
      in
      lines := !lines @ [status_line];

      (* Kind and size *)
      (match (task.kind, task.size) with
      | Some k, Some s ->
          lines := !lines @ [
            Printf.sprintf "Type: %s  Size: %s"
              (Task.task_kind_display k)
              (Task.task_size_display s)
          ]
      | Some k, None ->
          lines := !lines @ [Printf.sprintf "Type: %s" (Task.task_kind_display k)]
      | None, Some s ->
          lines := !lines @ [Printf.sprintf "Size: %s" (Task.task_size_display s)]
      | None, None -> ());

      (* Due date *)
      (match task.due_date with
      | Some (y, m, d) ->
          let date_str = Printf.sprintf "%04d-%02d-%02d" y m d in
          let colored_date =
            if Task.is_overdue task then
              Theme.colored Theme.Color.error date_str
            else if Task.is_due_today task then
              Theme.colored Theme.Color.warning date_str
            else date_str
          in
          lines := !lines @ [Printf.sprintf "Due: %s" colored_date]
      | None -> ());

      (* Start date *)
      (match task.start_date with
      | Some (y, m, d) ->
          lines := !lines @ [Printf.sprintf "Start: %04d-%02d-%02d" y m d]
      | None -> ());

      (* Completed at *)
      (match task.completed_at with
      | Some t ->
          lines := !lines @ [
            Printf.sprintf "Completed: %s" (Task.ptime_to_rfc3339 t)
          ]
      | None -> ());

      (* Assignee *)
      (match task.assignee with
      | Some a -> lines := !lines @ [Printf.sprintf "Assignee: %s" a]
      | None -> ());

      (* Context URL *)
      (match task.context_url with
      | Some url ->
          let short_url = Theme.truncate (width - 5) url in
          lines := !lines @ [
            Printf.sprintf "URL: %s"
              (Theme.colored Theme.Color.primary short_url)
          ]
      | None -> ());

      lines := !lines @ [""];

      (* Notes *)
      (match task.notes with
      | Some notes when String.length notes > 0 ->
          lines := !lines @ [Theme.bold "Notes:"];
          (* Split notes into lines and truncate *)
          let note_lines = String.split_on_char '\n' notes in
          List.iter
            (fun line ->
              let truncated = Theme.truncate width line in
              lines := !lines @ [truncated])
            note_lines
      | _ -> ());

      lines := !lines @ [""];

      (* Tags *)
      if List.length task.tags > 0 then (
        lines := !lines @ [Theme.bold "Tags:"];
        let tag_str = String.concat ", " task.tags in
        lines := !lines @ [Theme.truncate width tag_str]);

      (* Metadata *)
      if List.length task.metadata > 0 then (
        lines := !lines @ [""; Theme.bold "Metadata:"];
        List.iter
          (fun (key, value) ->
            let line = Printf.sprintf "  %s: %s" key value in
            lines := !lines @ [Theme.truncate width line])
          task.metadata);

      (* Timestamps *)
      lines := !lines @ [""];
      lines := !lines @ [
        Theme.dim (Printf.sprintf "Created: %s" (Task.ptime_to_rfc3339 task.created_at))
      ];
      lines := !lines @ [
        Theme.dim (Printf.sprintf "Updated: %s" (Task.ptime_to_rfc3339 task.updated_at))
      ];

      !lines
