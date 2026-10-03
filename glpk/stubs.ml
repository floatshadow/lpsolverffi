open Lp_common

type t
type flt = float
type row = flt lprow
type column = flt lpcolumn

external create : unit -> t = "ocaml_lp_glpk_create"
external number_rows : t -> int = "ocaml_lp_glpk_number_rows"
external number_columns : t -> int = "ocaml_lp_glpk_number_columns"
external number_elements : t -> int = "ocaml_lp_glpk_number_elements"
external add_rows : t -> row array -> int -> unit = "ocaml_lp_glpk_add_rows"
external add_columns : t -> column array -> int -> unit = "ocaml_lp_glpk_add_columns"
external objective_coefficients : t -> flt array = "ocaml_lp_glpk_objective_coefficients"
external change_objective_coefficients : t -> flt array -> unit
  = "ocaml_lp_glpk_change_objective_coefficients"
external set_direction : t -> direction -> unit = "ocaml_lp_glpk_set_direction"
external status : t -> status = "ocaml_lp_glpk_status"
external primal : t -> unit = "ocaml_lp_glpk_primal"
external primal_column_solution : t -> flt array = "ocaml_lp_glpk_primal_column_solution"
external set_log_level : t -> int -> unit = "ocaml_lp_glpk_set_log_level"
external objective_value : t -> flt = "ocaml_lp_glpk_objective_value"
external initial_solve : t -> unit = "ocaml_lp_glpk_initial_solve"
external read_mps : t -> string -> unit = "ocaml_lp_glpk_read_mps"
external write_mps : t -> string -> unit = "ocaml_lp_glpk_write_mps"
