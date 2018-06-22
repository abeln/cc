(* CPS conversion *)

module Gensym = struct
    let c = ref 0

    let prefix = "tv"

    let new_var () = 
        let c' = !c in
        c := !c + 1;
        Printf.sprintf "%s%d" prefix c'
end

let primop_to_cps = function
| Lam.Plus -> Cps.Plus
| Lam.Minus -> Cps.Minus
| Lam.Div -> Cps.Div
| Lam.Times -> Cps.Times
| Lam.Neg -> Cps.Neg
| Lam.Gt -> Cps.Gt
| Lam.Ge -> Cps.Ge
| Lam.Lt -> Cps.Lt
| Lam.Le -> Cps.Le
| op -> raise (Invalid_argument (Printf.sprintf !"can't convert lambda primop to cps primop: %{sexp:Lam.primop}" op))

let rec to_cps lexp cont =
    match lexp with
    | Lam.Var v -> cont (Cps.Var v)
    | Lam.Int i -> cont (Cps.Int i)
    | Lam.String s -> cont (Cps.String s) (* didn't do the single-char optimization *)
    | Lam.Record fs -> to_cps_lst fs (fun vs -> 
        let b = Gensym.new_var () in
        Cps.Record (List.map (fun v -> (v, Cps.Offp 0)) vs, b, cont (Cps.Var b))
    )
    | Lam.Select (i, e) -> to_cps e (fun v -> 
        let b = Gensym.new_var () in
        Cps.Select (i, v, b, cont (Cps.Var b))
    )
    | Lam.Fn (v, e) ->
        let (f, k) = (Gensym.new_var (), Gensym.new_var ()) in
        Cps.Fix ([(f, [v; k], to_cps e (fun z -> Cps.App (Var k, [z])))], cont (Cps.Var f))
    | Lam.Fix (fs, bs, e) ->
        let rec to_cps_fns fs bs =
            match (fs, bs) with
            | (f :: fr, Lam.Fn (a, b) :: br) -> (
                let k = Gensym.new_var () in
                (f, [a; k], to_cps b (fun bv -> Cps.App (Cps.Var k, [bv]))) :: to_cps_fns fr br
                )
            | ([], []) -> []
            | _ -> raise (Invalid_argument (Printf.sprintf !"can't convert mutually recursive functions: %{sexp:Lam.lexp}" lexp))
        in
        Cps.Fix (to_cps_fns fs bs, to_cps e (fun ev -> cont ev))
    | Lam.App (Lam.Prim op, e) -> to_cps_prim op e cont
    | Lam.App (f, e) ->
        let (k, a) = (Gensym.new_var (), Gensym.new_var ()) in
        Cps.Fix ([(k, [a], cont (Cps.Var a))],
            to_cps f (fun fv -> to_cps e (fun ev -> Cps.App (fv, [ev; Cps.Var k]))))
    | Lam.Prim op ->
        let x = Gensym.new_var () in
        to_cps (Lam.Fn (x, Lam.App (Lam.Prim op, Lam.Var x))) cont
    | _ -> raise (Invalid_argument (Printf.sprintf !"can't handle given lambda expression: %{sexp:Lam.lexp}" lexp))

and to_cps_lst lexps cont =
    let rec to_cps_rec lexps acc cont =
        match lexps with
        | [] -> cont (List.rev acc)
        | e :: es -> to_cps e (fun v -> to_cps_rec es (v :: acc) cont)
    in
    to_cps_rec lexps [] cont

and to_cps_prim op e cont =
        match op with
        | Lam.Neg ->
            let b = Gensym.new_var () in
            to_cps e (fun v -> Cps.Primop (primop_to_cps op, [v], [b], [cont (Var b)]))
        | Lam.Plus | Lam.Minus | Lam.Times | Lam.Div -> (
            match e with
            | Lam.Record fs ->
                let b = Gensym.new_var () in
                to_cps_lst fs (fun vs -> Cps.Primop (primop_to_cps op, vs, [b], [cont (Var b)]))
            | _ -> raise (Invalid_argument (Printf.sprintf !"expected argument to multi-arg primop to be a record, but got: %{sexp:Lam.lexp}" e))
        )
        | Lam.Gt | Lam.Ge | Lam.Lt | Lam.Le -> (
            match e with
            | Lam.Record fs ->
                let (fn, fa) = (Gensym.new_var (), Gensym.new_var ()) in
                to_cps_lst fs (fun vs ->
                    Cps.Fix ([(fn, [fa], cont (Cps.Var fa))],
                        Cps.Primop (primop_to_cps op, vs, [], [Cps.App ((Cps.Var fn), [Cps.Int 1]); Cps.App (Cps.Var fn, [Cps.Int 0])])))
            | _ -> raise (Invalid_argument (Printf.sprintf !"expected argument to multi-arg primop to be a record, but got: %{sexp:Lam.lexp}" e))
        )
        | _ -> raise (Invalid_argument (Printf.sprintf !"cannot cps-convert operator: %{sexp:Lam.primop}" op))
