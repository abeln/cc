open Sexplib.Std

type var = string [@@deriving sexp_of]

type accesspath =
    | Offp of int
    | Selp of int * accesspath
    [@@deriving sexp_of]

type primop =
    | Plus
    | Minus
    | Times
    | Div
    [@@deriving sexp_of]
    (* TODO: add rest *)

type lexp =
    | Var of var
    | Fn of var * lexp
    | Fix of var list * lexp list * lexp
    | App of lexp * lexp
    | Int of int
    | Real of string
    | String of string
    | Record of lexp list
    | Select of int * lexp
    | Prim of primop
    [@@deriving sexp_of]

let lexp_to_string e = sexp_of_lexp e |> Sexplib.Sexp.to_string_hum