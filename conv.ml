(* CPS conversion *)

module Gensym = struct
    let res_c = ref 0
    let fun_c = ref 0
    let arg_c = ref 0

    let res_pre = "res"
    let fun_pre = "fun"
    let arg_pre = "arg"

    let inc cnt pre = fun () ->
        let cnt' = !cnt in
        cnt := !cnt + 1;
        Printf.sprintf "%s%d" pre cnt'

    let new_res = inc res_c res_pre
    let new_fun = inc fun_c fun_pre
    let new_arg = inc arg_c arg_pre
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
        let b = Gensym.new_res () in
        Cps.Record (List.map (fun v -> (v, Cps.Offp 0)) vs, b, cont (Cps.Var b))
    )
    | Lam.Select (i, e) -> to_cps e (fun v -> 
        let b = Gensym.new_res () in
        Cps.Select (i, v, b, cont (Cps.Var b))
    )
    | Lam.Fn (v, e) ->
        let (f, k) = (Gensym.new_fun (), Gensym.new_arg ()) in
        Cps.Fix ([(f, [v; k], to_cps e (fun z -> Cps.App (Var k, [z])))], cont (Cps.Var f))
    | Lam.Fix (fs, bs, e) ->
        let rec to_cps_fns fs bs =
            match (fs, bs) with
            | (f :: fr, Lam.Fn (a, b) :: br) -> (
                let k = Gensym.new_arg () in
                (f, [a; k], to_cps b (fun bv -> Cps.App (Cps.Var k, [bv]))) :: to_cps_fns fr br
                )
            | ([], []) -> []
            | _ -> raise (Invalid_argument (Printf.sprintf !"can't convert mutually recursive functions: %{sexp:Lam.lexp}" lexp))
        in
        Cps.Fix (to_cps_fns fs bs, to_cps e (fun ev -> cont ev))
    | Lam.App (Lam.Prim op, e) -> to_cps_prim op e cont
    | Lam.App (f, e) ->
        let (k, a) = (Gensym.new_fun (), Gensym.new_arg ()) in
        Cps.Fix ([(k, [a], cont (Cps.Var a))],
            to_cps f (fun fv -> to_cps e (fun ev -> Cps.App (fv, [ev; Cps.Var k]))))
    | Lam.Prim op ->
        let x = Gensym.new_arg () in
        to_cps (Lam.Fn (x, Lam.App (Lam.Prim op, Lam.Var x))) cont
    | Lam.Con (rep, e) -> (
        match rep with 
        | Lam.Constant i -> to_cps (Lam.Int i) cont
        | Lam.Tagged i -> to_cps (Lam.Record [e; Lam.Int i]) cont
    )
    | Lam.Decon (rep, e) -> (
        match rep with
        | Lam.Tagged _ -> to_cps (Lam.Select (0, e)) cont
        | Lam.Constant _ -> raise (Invalid_argument (Printf.sprintf !"can't apply decon to constant constructor: %{sexp:Lam.conrep}" rep))
    )
    | Lam.If (c, t, f) ->
        (to_cps (Lam.Switch (c, [],
            [(Lam.Datacon (Lam.Constant 1), t);
             (Lam.Datacon (Lam.Constant 0), f)], None)) cont)
    | Lam.Switch (e, _, cs, def) -> (
        let ev2 = Gensym.new_res () in
        to_cps e (fun ev ->
            let (k, a) = (Gensym.new_fun (), Gensym.new_arg ()) in 
            Cps.Fix ([(k, [a], to_cps_switch (Cps.Var a) cs def cont)],
                Cps.Primop (Cps.Boxed, [ev], [],
                    [Cps.Select (0, ev, ev2, Cps.App (Cps.Var k, [Cps.Var ev2]));
                     Cps.App (Cps.Var k, [ev])])))
    )
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
            let b = Gensym.new_res () in
            to_cps e (fun v -> Cps.Primop (primop_to_cps op, [v], [b], [cont (Var b)]))
        | Lam.Plus | Lam.Minus | Lam.Times | Lam.Div -> (
            match e with
            | Lam.Record fs ->
                let b = Gensym.new_res () in
                to_cps_lst fs (fun vs -> Cps.Primop (primop_to_cps op, vs, [b], [cont (Var b)]))
            | _ -> raise (Invalid_argument (Printf.sprintf !"expected argument to multi-arg primop to be a record, but got: %{sexp:Lam.lexp}" e))
        )
        | Lam.Gt | Lam.Ge | Lam.Lt | Lam.Le -> (
            match e with
            | Lam.Record fs ->
                let (fn, fa) = (Gensym.new_fun (), Gensym.new_arg ()) in
                to_cps_lst fs (fun vs ->
                    Cps.Fix ([(fn, [fa], cont (Cps.Var fa))],
                        Cps.Primop (primop_to_cps op, vs, [], [Cps.App ((Cps.Var fn), [Cps.Int 1]); Cps.App (Cps.Var fn, [Cps.Int 0])])))
            | _ -> raise (Invalid_argument (Printf.sprintf !"expected argument to multi-arg primop to be a record, but got: %{sexp:Lam.lexp}" e))
        )
        | _ -> raise (Invalid_argument (Printf.sprintf !"cannot cps-convert operator: %{sexp:Lam.primop}" op))

and to_cps_switch v cs def cont =
    let (cont', a) = (Gensym.new_fun (), Gensym.new_arg ()) in
    let ifeq v1 v2 c1 c2 = 
        Cps.Primop (Cps.Ieq, [v1; v2], [], [c1; c2])
    in
    let rec switch_one = function
    | (cr, e) :: cs' ->
        let rem_cexp = switch_one cs' in
        to_cps e (fun ev ->
            match cr with
            | Lam.Intcon i | Lam.Datacon (Lam.Constant i)| Lam.Datacon (Lam.Tagged i)  ->
                ifeq (Cps.Int i) v (Cps.App (Var cont', [ev])) rem_cexp
            | _ -> raise (Invalid_argument (Printf.sprintf !"can't handle constructor: %{sexp:Lam.con}" cr))
            )
    | [] -> (
        match def with
        | Some def_exp -> to_cps def_exp (fun def_v -> Cps.App (Cps.Var cont', [def_v]))
        | None -> (Cps.App (Cps.Var cont', [Cps.Int 0])) (* should throw here instead *)
    )
    in
    Cps.Fix ([(cont', [a], cont (Cps.Var a))],
        switch_one cs)
