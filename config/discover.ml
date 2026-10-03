module C = Configurator.V1

let write_file path contents =
  let channel = open_out path in
  Fun.protect ~finally:(fun () -> close_out channel) (fun () -> output_string channel contents)

let getenv name = Option.value (Sys.getenv_opt name) ~default:""
let flags name = C.Flags.extract_blank_separated_words (getenv name)

let detect c name =
  let variable suffix = "LP_" ^ String.uppercase_ascii name ^ suffix in
  let mode = match getenv (variable "") with "" -> "auto" | mode -> mode in
  if not (List.mem mode [ "auto"; "enabled"; "disabled" ]) then
    C.die "%s must be auto, enabled or disabled" (variable "");
  let headers, libs, body =
    match name with
    | "clp" ->
        ( [ "coin/Clp_C_Interface.h"; "Clp_C_Interface.h" ],
          [ "-lClp"; "-lCoinUtils" ],
          "Clp_Simplex *m = Clp_newModel(); Clp_initialSolve(m); Clp_deleteModel(m);" )
    | "highs" ->
        ( [ "highs/interfaces/highs_c_api.h"; "interfaces/highs_c_api.h"; "highs_c_api.h" ],
          [ "-lhighs" ],
          "void *m = Highs_create(); Highs_run(m); Highs_destroy(m);" )
    | "glpk" ->
        ( [ "glpk.h" ],
          [ "-lglpk" ],
          "glp_prob *m = glp_create_prob(); glp_simplex(m, 0); glp_delete_prob(m);" )
    | _ -> C.die "Unknown backend: %s" name
  in
  let empty : C.Pkg_config.package_conf = { cflags = []; libs = [] } in
  let found =
    if mode = "disabled" then None
    else
      let package =
        match C.Pkg_config.get c with
        | None -> None
        | Some pc -> C.Pkg_config.query pc ~package:name
      in
      let prefix = getenv (variable "_PREFIX") in
      let prefix_cflags, prefix_libs =
        if prefix = "" then ([], [])
        else
          ( [
              "-I" ^ Filename.concat prefix "include";
              "-I" ^ Filename.concat prefix ("include/" ^ name);
            ],
            [ "-L" ^ Filename.concat prefix "lib"; "-L" ^ Filename.concat prefix "lib64" ] )
      in
      let candidates =
        let fallback = { C.Pkg_config.cflags = []; libs } in
        match package with None -> [ fallback ] | Some p -> [ p; fallback ]
      in
      let candidates =
        List.concat_map
          (fun (conf : C.Pkg_config.package_conf) ->
            let cflags = flags (variable "_CFLAGS") @ prefix_cflags @ conf.cflags in
            let libs =
              match Sys.getenv_opt (variable "_LIBS") with
              | Some _ -> flags (variable "_LIBS")
              | None -> conf.libs
            in
            let conf = { C.Pkg_config.cflags; libs = prefix_libs @ libs } in
            if name = "highs" then
              [
                conf;
                { conf with cflags = cflags @ [ "-I/usr/include/highs" ] };
                { conf with cflags = cflags @ [ "-I/usr/local/include/highs" ] };
              ]
            else [ conf ])
          candidates
      in
      List.find_map
        (fun (conf : C.Pkg_config.package_conf) ->
          List.find_map
            (fun header ->
              let include_line = "#include <" ^ header ^ ">\n" in
              if
                C.c_test c ~c_flags:conf.cflags ~link_flags:conf.libs
                  (include_line ^ "int main(void) { " ^ body ^ " return 0; }\n")
              then Some (conf, include_line)
              else None)
            headers)
        candidates
  in
  let conf, header, available =
    match found with
    | Some (conf, header) ->
        Printf.eprintf "lp: %s enabled (compile/link check passed)\n%!" name;
        (conf, header, true)
    | None when mode = "enabled" ->
        C.die
          "%s requested but its headers/library could not be compiled and linked; set %s, %s or %s"
          name (variable "_PREFIX") (variable "_CFLAGS") (variable "_LIBS")
    | None ->
        Printf.eprintf "lp: %s disabled (%s)\n%!" name
          (if mode = "disabled" then "requested" else "compile/link check failed");
        (empty, "", false)
  in
  write_file (name ^ "_available") (string_of_bool available);
  write_file (name ^ "_header.h") header;
  C.Flags.write_sexp (name ^ "_c_flags.sexp") conf.cflags;
  C.Flags.write_sexp (name ^ "_c_library_flags.sexp") conf.libs

let () = C.main ~name:"lp" (fun c -> List.iter (detect c) [ "clp"; "highs"; "glpk" ])
