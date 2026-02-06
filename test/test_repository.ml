(** Repository tests *)

open Phitodo

let test_db_path = "/tmp/phitodo_test.db"

let setup () =
  (* Remove existing test database *)
  if Sys.file_exists test_db_path then Sys.remove test_db_path;
  (* Open database and initialize schema *)
  Repository.open_db test_db_path

let teardown db =
  Repository.close_db db;
  if Sys.file_exists test_db_path then Sys.remove test_db_path

let test_task_crud () =
  let db = setup () in

  (* Create a task *)
  let task = Task.create ~title:"Test task" () in
  Repository.Task.insert db task;

  (* Read all tasks *)
  let tasks = Repository.Task.get_all db in
  Alcotest.(check int) "one task" 1 (List.length tasks);

  (* Read specific task *)
  let found = Repository.Task.get db task.id in
  Alcotest.(check bool) "task found" true (Option.is_some found);
  let found = Option.get found in
  Alcotest.(check string) "title matches" "Test task" found.title;

  (* Update task *)
  let updated = { found with title = "Updated task" } in
  Repository.Task.update db updated;
  let found = Repository.Task.get db task.id |> Option.get in
  Alcotest.(check string) "updated title" "Updated task" found.title;

  (* Delete task *)
  Repository.Task.delete db task.id;
  let found = Repository.Task.get db task.id in
  Alcotest.(check bool) "task deleted" true (Option.get found).deleted;

  teardown db

let test_project_crud () =
  let db = setup () in

  (* Default inbox project should exist *)
  let projects = Repository.Project.get_all db in
  Alcotest.(check bool) "inbox exists" true
    (List.exists (fun (p : Project.t) -> p.is_inbox) projects);

  (* Create a project *)
  let project = Project.create ~name:"Test project" () in
  Repository.Project.insert db project;

  (* Read all projects *)
  let projects = Repository.Project.get_all db in
  Alcotest.(check int) "two projects" 2 (List.length projects);

  (* Read specific project *)
  let found = Repository.Project.get db project.id in
  Alcotest.(check bool) "project found" true (Option.is_some found);

  teardown db

let test_tag_crud () =
  let db = setup () in

  (* Create a tag *)
  let tag = Tag.create ~name:"Test tag" () in
  Repository.Tag.insert db tag;

  (* Read all tags *)
  let tags = Repository.Tag.get_all db in
  Alcotest.(check int) "one tag" 1 (List.length tags);

  (* Read specific tag *)
  let found = Repository.Tag.get db tag.id in
  Alcotest.(check bool) "tag found" true (Option.is_some found);

  teardown db

let test_task_tags () =
  let db = setup () in

  (* Create a tag *)
  let tag = Tag.create ~name:"Important" () in
  Repository.Tag.insert db tag;

  (* Create a task with tag *)
  let task = Task.create ~title:"Tagged task" ~tags:[tag.id] () in
  Repository.Task.insert db task;

  (* Read task and verify tags *)
  let found = Repository.Task.get db task.id |> Option.get in
  Alcotest.(check int) "one tag" 1 (List.length found.tags);
  Alcotest.(check string) "tag id" tag.id (List.hd found.tags);

  teardown db

let test_task_counts () =
  let db = setup () in

  (* Create tasks with different statuses *)
  let task1 = Task.create ~title:"Inbox task" ~status:Task.Inbox () in
  let task2 = Task.create ~title:"Active task" ~status:Task.Active () in
  let task3 = Task.create ~title:"Completed task" ~status:Task.Completed () in
  Repository.Task.insert db task1;
  Repository.Task.insert db task2;
  Repository.Task.insert db task3;

  (* Count by status *)
  let inbox_count = Repository.Task.count_by_status db Task.Inbox in
  let active_count = Repository.Task.count_by_status db Task.Active in
  let completed_count = Repository.Task.count_by_status db Task.Completed in

  Alcotest.(check int64) "inbox count" 1L inbox_count;
  Alcotest.(check int64) "active count" 1L active_count;
  Alcotest.(check int64) "completed count" 1L completed_count;

  teardown db

let () =
  Alcotest.run "Repository"
    [
      ("Task CRUD", [ Alcotest.test_case "task_crud" `Quick test_task_crud ]);
      ("Project CRUD", [ Alcotest.test_case "project_crud" `Quick test_project_crud ]);
      ("Tag CRUD", [ Alcotest.test_case "tag_crud" `Quick test_tag_crud ]);
      ("Task Tags", [ Alcotest.test_case "task_tags" `Quick test_task_tags ]);
      ("Task Counts", [ Alcotest.test_case "task_counts" `Quick test_task_counts ]);
    ]
