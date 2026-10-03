open Lp_common

let close expected actual =
  if abs_float (expected -. actual) > 1e-6 || not (Float.is_finite actual) then
    failwith (Printf.sprintf "expected %.9g, got %.9g" expected actual)

let invalid f =
  match f () with () -> failwith "expected Invalid_argument" | exception Invalid_argument _ -> ()

let run (module S : SOLVER with type flt = float) =
  let column obj elements =
    { column_obj = obj; column_lower = 0.; column_upper = infinity; column_elements = elements }
  in
  let row lower upper elements =
    { row_lower = lower; row_upper = upper; row_elements = elements }
  in
  let create () =
    let t = S.create () in
    S.set_log_level t 0;
    t
  in
  let optimum t value =
    assert (S.status t = Optimal);
    close value (S.objective_value t)
  in
  let t = create () in
  assert (S.number_rows t = 0 && S.number_columns t = 0 && S.number_elements t = 0);
  assert (S.objective_coefficients t = [||]);
  S.add_rows t [||] 0;
  S.add_columns t [||] 0;
  S.change_objective_coefficients t [||];
  S.add_columns t [| column 1. [||]; column 1. [||]; column 99. [| (-1, nan) |] |] 2;
  S.add_rows t
    [|
      row 4. infinity [| (0, 2.); (1, 1.) |];
      row 4. infinity [| (0, 1.); (1, 2.) |];
      row nan nan [||];
    |]
    2;
  assert (S.number_rows t = 2 && S.number_columns t = 2 && S.number_elements t = 4);
  S.set_direction t Minimize;
  S.solve_with_log_level t 0;
  optimum t (8. /. 3.);
  Array.iter (close (4. /. 3.)) (S.primal_column_solution t);
  S.change_objective_coefficients t [| 1.; 3. |];
  assert (S.objective_coefficients t = [| 1.; 3. |]);
  S.primal t;
  optimum t 4.;
  S.add_rows t [| row neg_infinity 5. [| (0, 1.); (1, 1.) |] |] 1;
  S.set_direction t Maximize;
  S.initial_solve t;
  optimum t 15.;
  invalid (fun () -> S.add_rows t [||] 1);
  invalid (fun () -> S.add_columns t [||] (-1));
  invalid (fun () -> S.add_rows t [| row 0. 1. [| (2, 1.) |] |] 1);
  invalid (fun () -> S.add_columns t [| column 1. [| (-1, 1.) |] |] 1);
  invalid (fun () -> S.change_objective_coefficients t [| 1. |]);
  let path = Filename.temp_file "lp-test-" ".mps" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      S.set_direction t Minimize;
      S.write_mps t path;
      let copy = create () in
      S.read_mps copy path;
      S.set_direction copy Minimize;
      S.initial_solve copy;
      optimum copy 4.);
  (* Construct the transpose order: rows first, then sparse columns. *)
  let t = create () in
  S.add_rows t [| row 4. infinity [||]; row 4. infinity [||] |] 2;
  S.add_columns t [| column 1. [| (0, 2.); (1, 1.) |]; column 1. [| (0, 1.); (1, 2.) |] |] 2;
  S.set_direction t Minimize;
  S.initial_solve t;
  optimum t (8. /. 3.);
  let t = create () in
  S.add_columns t [| { (column 1. [||]) with column_upper = 1. } |] 1;
  S.add_rows t [| row 2. infinity [| (0, 1.) |] |] 1;
  S.initial_solve t;
  assert (S.status t = Infeasible);
  let t = create () in
  S.add_columns t [| column (-1.) [||] |] 1;
  S.set_direction t Minimize;
  S.initial_solve t;
  assert (S.status t = Unbounded || S.status t = Inf_or_unbd);
  Gc.full_major ()
