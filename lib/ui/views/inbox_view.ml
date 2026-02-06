(** Inbox view *)

include Task_view

let create () = Task_view.create ~title:"Inbox" ()

let filter_tasks tasks = Filter_service.filter_inbox tasks
