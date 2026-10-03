module Glpk = struct
  include Stubs

  let solve_with_log_level t l =
    set_log_level t l;
    initial_solve t
end
