ppx_nolazy
==========

A ppx rewriter to be able to not use lazy patterns for match and function
constructions.

Overview
========

This rewriter is to be used with record types that contains only lazy fields.
It allows the user to not specify lazy patterns when matching on the fields:

```ocaml
type op = [ `OpPlus | `OpMinus | `OpMult ]
and field_unop = {op: operator Lazy.t; expr: expr Lazy.t}
and ident_field = {text: string Lazy.t}
and expr = [ `UnOp of field_unop | `Ident of ident_field ]
```

It rebinds `match%nolazy` and `function%nolazy`

Instead of writing a pattern like:

```ocaml
match expr with
| `Unop {op= (lazy `OpMinus); expr= (lazy (`UnOp {op= lazy `OpMinus; expr= lazy unop_expr}))} ->
    unop_expr
| _ ->
    expr
```

With the rewriter, one can write the same pattern, avoiding the lazy patterns:

```ocaml
match%nolazy expr with
| `Unop {op= `OpMinus; expr= `UnOp {op= `OpMinus; expr= unop_expr}} ->
    unop_expr
| _ ->
    expr
```
