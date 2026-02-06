(** Anytime view - tasks without due dates *)

include Task_view

let create () = Task_view.create ~title:"Anytime" ()

let filter_tasks tasks = Filter_service.filter_anytime tasks
