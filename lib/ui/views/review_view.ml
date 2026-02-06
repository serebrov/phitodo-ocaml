(** Review view - overdue tasks *)

include Task_view

let create () = Task_view.create ~title:"Review" ()

let filter_tasks tasks = Filter_service.filter_review tasks
