type t

type direction = Maximize | Minimize

type status =
  | Optimal  (** primal feasible and bounded, dual feasible and bounded *)
  | Infeasible  (** primal infeasible, dual unbounded or infeasible *)
  | Unbounded  (** primal unbounded, dual infeasible *)
  | Inf_or_unbd
  | Suboptimal
  | Other

type 'a lprow = { row_lower : 'a; row_upper : 'a; row_elements : (int * 'a) array }

type 'a lpcolumn = {
  column_obj : 'a;
  column_lower : 'a;
  column_upper : 'a;
  column_elements : (int * 'a) array;
}

module type SOLVER = sig
  type t
  type flt
  type row = flt lprow
  type column = flt lpcolumn

  val create : unit -> t
  val number_rows : t -> int
  val number_columns : t -> int
  val number_elements : t -> int
  val add_rows : t -> row array -> int -> unit
  val add_columns : t -> column array -> int -> unit
  val objective_coefficients : t -> flt array
  val change_objective_coefficients : t -> flt array -> unit
  val set_direction : t -> direction -> unit
  val status : t -> status
  val primal : t -> unit
  val primal_column_solution : t -> flt array
  val set_log_level : t -> int -> unit
  val objective_value : t -> flt
  val initial_solve : t -> unit
  val solve_with_log_level : t -> int -> unit
  val read_mps : t -> string -> unit
  val write_mps : t -> string -> unit
end
