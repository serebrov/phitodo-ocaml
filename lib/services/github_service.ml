(** GitHub API service - stub implementation *)

open Github_view

(** GitHub data result *)
type github_data = {
  review_prs : github_item list;
  my_prs : github_item list;
  assigned_issues : github_item list;
}

(** Fetch all GitHub data - stub that returns empty data *)
let fetch_all ~token:_ =
  Ok { review_prs = []; my_prs = []; assigned_issues = [] }

(** Sync GitHub data to local tasks - stub *)
let sync_to_tasks ~db:_ _github_data = ()
