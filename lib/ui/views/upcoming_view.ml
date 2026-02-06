(** Upcoming view - tasks with future due dates *)

include Task_view

let create () = Task_view.create ~title:"Upcoming" ()

let filter_tasks tasks = Filter_service.filter_upcoming tasks
