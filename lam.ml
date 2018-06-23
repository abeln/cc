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
    | Neg
    | Gt
    | Ge
    | Lt
    | Le
    [@@deriving sexp_of]
    (* TODO: add rest *)

type conrep =
    | Tagged of int
    | Constant of int
    [@@deriving sexp_of]

type con =
    | Datacon of conrep
    | Intcon of int
    | Stringcon of string 
    [@@deriving sexp_of]

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
    | Switch of lexp * conrep list * (con * lexp) list * lexp option
    | Decon of conrep * lexp
    | Con of conrep * lexp
    | If of lexp * lexp * lexp
    [@@deriving sexp_of]