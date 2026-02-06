(** Main library module - re-export all public modules *)

(* Models *)
module Task = Task
module Project = Project
module Tag = Tag

(* Error handling *)
module Error = Error

(* Database *)
module Schema = Schema
module Repository = Repository

(* Configuration *)
module Config = Config

(* Services *)
module Filter_service = Filter_service
module Github_service = Github_service
module Toggl_service = Toggl_service

(* UI Theme *)
module Theme = Theme

(* UI Components *)
module Sidebar = Sidebar
module Task_list = Task_list
module Task_detail = Task_detail
module Task_form = Task_form
module Modal = Modal
module Input = Input
module Help = Help

(* UI Views *)
module Task_view = Task_view
module Inbox_view = Inbox_view
module Today_view = Today_view
module Upcoming_view = Upcoming_view
module Anytime_view = Anytime_view
module Completed_view = Completed_view
module Review_view = Review_view
module Project_view = Project_view
module Tag_view = Tag_view
module Settings_view = Settings_view
module Github_view = Github_view
module Toggl_view = Toggl_view

(* Application *)
module App = App
module Update = Update
module View = View
