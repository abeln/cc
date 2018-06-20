open Sexplib.Std

type var = string [@@deriving sexp_of]

type value =
    | Var of var
    | Label of var
    | Int of int
    | Real of string
    | String of string
    [@@deriving sexp_of]

type primop =
    | Plus
    | Minus
    | Times
    | Div
    [@@deriving sexp_of]
    (* TODO: add rest *)

type accesspath =
    | Offp of int
    | Selp of int * accesspath
    [@@deriving sexp_of]


type cexp = 
    | Record of (value * accesspath) list * var * cexp
    | Select of int * value * var * cexp
    | Offset of int * value * var * cexp
    | App of value * (value list)
    | Fix of (var * var list * cexp) list * cexp
    | Switch of value * cexp list
    | Primop of primop * value list * var list * cexp list
    [@@deriving sexp_of]

let cexp_to_string e = sexp_of_cexp e |> Sexplib.Sexp.to_string_hum
