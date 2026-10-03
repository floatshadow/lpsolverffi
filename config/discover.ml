module C = Configurator.V1

let write_file path contents =
  let channel = open_out path in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () -> output_string channel contents)

let getenv name = Option.value (Sys.getenv_opt name) ~default:""
let flags name = C.Flags.extract_blank_separated_words (getenv name)
let variable name suffix = "LP_" ^ String.uppercase_ascii name ^ suffix

let with_overrides name (conf : C.Pkg_config.package_conf) =
  let prefix = getenv (variable name "_PREFIX") in
  let include_dirs, library_dirs =
    if prefix = "" then ([], []) else ([ "include"; "include/" ^ name ], [ "lib"; "lib64" ])
  in
  let cflags =
    flags (variable name "_CFLAGS")
    @ List.map (fun dir -> "-I" ^ Filename.concat prefix dir) include_dirs
    @ conf.cflags
  in
  let libs =
    match Sys.getenv_opt (variable name "_LIBS") with
    | Some _ -> flags (variable name "_LIBS")
    | None -> conf.libs
  in
  {
    C.Pkg_config.cflags;
    libs = List.map (fun dir -> "-L" ^ Filename.concat prefix dir) library_dirs @ libs;
  }

let candidates c name libs =
  let fallback = { C.Pkg_config.cflags = []; libs } in
  let package =
    match C.Pkg_config.get c with None -> None | Some pc -> C.Pkg_config.query pc ~package:name
  in
  let configs = match package with None -> [ fallback ] | Some conf -> [ conf; fallback ] in
  List.map (with_overrides name) configs

let probe c configs headers body =
  List.find_map
    (fun (conf : C.Pkg_config.package_conf) ->
      List.find_map
        (fun header ->
          let include_line = "#include <" ^ header ^ ">\n" in
          let source = include_line ^ "int main(void) { " ^ body ^ " return 0; }\n" in
          if C.c_test c ~c_flags:conf.cflags ~link_flags:conf.libs source then
            Some (conf, include_line)
          else None)
        headers)
    configs

let config_clp c =
  let configs = candidates c "clp" [ "-lClp"; "-lCoinUtils" ] in
  let headers = [ "coin/Clp_C_Interface.h"; "Clp_C_Interface.h" ] in
  let body = "Clp_Simplex *m = Clp_newModel(); Clp_initialSolve(m); Clp_deleteModel(m);" in
  probe c configs headers body

let config_highs c =
  let configs = candidates c "highs" [ "-lhighs" ] in
  let configs =
    List.concat_map
      (fun (conf : C.Pkg_config.package_conf) ->
        [
          conf;
          { conf with cflags = conf.cflags @ [ "-I/usr/include/highs" ] };
          { conf with cflags = conf.cflags @ [ "-I/usr/local/include/highs" ] };
        ])
      configs
  in
  let headers = [ "highs/interfaces/highs_c_api.h"; "interfaces/highs_c_api.h"; "highs_c_api.h" ] in
  let body = "void *m = Highs_create(); Highs_run(m); Highs_destroy(m);" in
  probe c configs headers body

let config_glpk c =
  let configs = candidates c "glpk" [ "-lglpk" ] in
  let headers = [ "glpk.h" ] in
  let body = "glp_prob *m = glp_create_prob(); glp_simplex(m, 0); glp_delete_prob(m);" in
  probe c configs headers body

let write_config c name configure =
  let mode = match getenv (variable name "") with "" -> "auto" | mode -> mode in
  let found =
    match mode with
    | "disabled" -> None
    | "auto" | "enabled" -> configure c
    | _ -> C.die "%s must be auto, enabled or disabled" (variable name "")
  in
  let conf, header, available =
    match found with
    | Some (conf, header) ->
        Printf.eprintf "lp: %s enabled (compile/link check passed)\n%!" name;
        (conf, header, true)
    | None when mode = "enabled" ->
        C.die
          "%s requested but its headers/library could not be compiled and linked; set %s, %s or %s"
          name (variable name "_PREFIX") (variable name "_CFLAGS") (variable name "_LIBS")
    | None ->
        Printf.eprintf "lp: %s disabled (%s)\n%!" name
          (if mode = "disabled" then "requested" else "compile/link check failed");
        ({ C.Pkg_config.cflags = []; libs = [] }, "", false)
  in
  write_file (name ^ "_available") (string_of_bool available);
  write_file (name ^ "_header.h") header;
  C.Flags.write_sexp (name ^ "_c_flags.sexp") conf.cflags;
  C.Flags.write_sexp (name ^ "_c_library_flags.sexp") conf.libs

let () =
  C.main ~name:"lp" (fun c ->
      write_config c "clp" config_clp;
      write_config c "highs" config_highs;
      write_config c "glpk" config_glpk)
