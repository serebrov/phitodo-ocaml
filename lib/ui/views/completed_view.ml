(** Completed view - completed tasks *)

include Task_view

let create () = Task_view.create ~title:"Completed" ()

let filter_tasks tasks = Filter_service.filter_completed tasks
