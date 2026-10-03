(** Types *)

open Lp_common

type t
type flt = float
type row = float lprow
type column = float lpcolumn

(** Creating model *)

external create : unit -> t = "ocaml_lp_clp_create"

(** Getters and setters of problem parameters *)

external resize : t -> int -> int -> unit = "ocaml_lp_clp_resize"
external number_rows : t -> int = "ocaml_lp_clp_number_rows"
external number_columns : t -> int = "ocaml_lp_clp_number_columns"
external number_elements : t -> int = "ocaml_lp_clp_number_elements"
external direction : t -> direction = "ocaml_lp_clp_direction"
external set_direction : t -> direction -> unit = "ocaml_lp_clp_set_direction"
external add_rows : t -> row array -> int -> unit = "ocaml_lp_clp_add_rows"
external delete_rows : t -> int array -> unit = "ocaml_lp_clp_delete_rows"
external add_columns : t -> column array -> int -> unit = "ocaml_lp_clp_add_columns"
external delete_columns : t -> int array -> unit = "ocaml_lp_clp_delete_columns"
external row_lower : t -> float array = "ocaml_lp_clp_row_lower"
external change_row_lower : t -> float array -> unit = "ocaml_lp_clp_change_row_lower"
external row_upper : t -> float array = "ocaml_lp_clp_row_upper"
external change_row_upper : t -> float array -> unit = "ocaml_lp_clp_change_row_upper"
external column_lower : t -> float array = "ocaml_lp_clp_column_lower"
external change_column_lower : t -> float array -> unit = "ocaml_lp_clp_change_column_lower"
external column_upper : t -> float array = "ocaml_lp_clp_column_upper"
external change_column_upper : t -> float array -> unit = "ocaml_lp_clp_change_column_upper"
external objective_coefficients : t -> float array = "ocaml_lp_clp_objective_coefficients"

external change_objective_coefficients : t -> float array -> unit
  = "ocaml_lp_clp_change_objective_coefficients"

(** Getters and setters of solver parameters*)

external log_level : t -> int = "ocaml_lp_clp_log_level"
external set_log_level : t -> int -> unit = "ocaml_lp_clp_set_log_level"

(** Solver operations *)

external primal : t -> unit = "ocaml_lp_clp_primal"
external dual : t -> unit = "ocaml_lp_clp_dual"
external initial_solve : t -> unit = "ocaml_lp_clp_initial_solve"
external initial_primal : t -> unit = "ocaml_lp_clp_initial_primal"
external initial_dual : t -> unit = "ocaml_lp_clp_initial_dual"
external initial_barrier : t -> unit = "ocaml_lp_clp_initial_barrier"

(** Retrieving solutions *)

external objective_value : t -> float = "ocaml_lp_clp_objective_value"
external primal_row_solution : t -> float array = "ocaml_lp_clp_primal_row_solution"
external primal_column_solution : t -> float array = "ocaml_lp_clp_primal_column_solution"
external dual_row_solution : t -> float array = "ocaml_lp_clp_dual_row_solution"
external dual_column_solution : t -> float array = "ocaml_lp_clp_dual_column_solution"
external status : t -> status = "ocaml_lp_clp_status"

(** MPS operations *)

external read_mps : t -> string -> unit = "ocaml_lp_clp_read_mps"
external write_mps : t -> string -> unit = "ocaml_lp_clp_write_mps"
