(** Today view - tasks due today or overdue *)

include Task_view

let create () = Task_view.create ~title:"Today" ()

let filter_tasks tasks = Filter_service.filter_today tasks
