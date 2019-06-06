type op = [`OpPlus | `OpMinus | `OpMult]

and field_unop = {op: op Lazy.t; expr: expr Lazy.t}

and ident_field = {text: string Lazy.t}

and expr = [`UnOp of field_unop | `Ident of ident_field]

let f1 expr =
  match expr with
  | `UnOp
      { op= (lazy `OpMinus)
      ; expr= (lazy (`UnOp {op= (lazy `OpMinus); expr= (lazy unop_expr)})) } ->
      unop_expr
  | _ ->
      expr

let f2 expr =
  match%nolazy expr with
  | `UnOp {op= `OpMinus; expr= `UnOp {op= `OpMinus; expr= unop_expr}} ->
      unop_expr
  | _ ->
      expr

let expr =
  `UnOp
    { op= lazy `OpMinus
    ; expr=
        lazy (`UnOp {op= lazy `OpMinus; expr= lazy (`Ident {text= lazy "42"})})
    }

let () =
  ignore (f1 expr) ;
  ignore (f2 expr)
