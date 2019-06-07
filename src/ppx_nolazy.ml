open Ppxlib
open Ast_builder.Default

let rec rewrite_pattern pattern =
  let loc = pattern.ppat_loc in
  match pattern.ppat_desc with
  | Ppat_alias (pattern, alias) ->
      ppat_alias ~loc (rewrite_pattern pattern) alias
  | Ppat_tuple patterns ->
      ppat_tuple ~loc (List.map rewrite_pattern patterns)
  | Ppat_construct (ident_loc, Some pattern) ->
      ppat_construct ~loc ident_loc (Some (rewrite_pattern pattern))
  | Ppat_variant
      ( label
      , Some {ppat_desc= Ppat_record (ident_pat_list, closed_flag); ppat_loc}
      ) ->
      let aux (ident, pat) =
        let rewrited = rewrite_pattern pat in
        (ident, ppat_lazy ~loc:ident.loc rewrited)
      in
      ppat_variant ~loc label
        (Some
           (ppat_record ~loc:ppat_loc (List.map aux ident_pat_list) closed_flag))
  | Ppat_array patterns ->
      ppat_array ~loc:pattern.ppat_loc (List.map rewrite_pattern patterns)
  | Ppat_or (pat1, pat2) ->
      ppat_or ~loc (rewrite_pattern pat1) (rewrite_pattern pat2)
  | _ ->
      pattern

let rewrite_case case = {case with pc_lhs= rewrite_pattern case.pc_lhs}

let rewriter_cases cases = List.map rewrite_case cases

let expr_mapper expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_match (match_expr, match_cases) ->
      pexp_match ~loc match_expr (rewriter_cases match_cases)
  | Pexp_function function_cases ->
      pexp_function ~loc (rewriter_cases function_cases)
  | _ ->
      Location.raise_errorf ~loc
        "nolazy can only be used with 'match', and 'function'"

let mapper =
  Extension.declare_with_path_arg "nolazy" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg:_ expr -> expr_mapper expr)

let () = Driver.register_transformation "nolazy" ~extensions:[mapper]
