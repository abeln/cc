(* CPS conversion *)

module Gensym = struct
    let c = ref 0

    let prefix = "tv"

    let genvar () = 
        let c' = !c in
        c := !c + 1;
        Printf.sprintf "%s%d" prefix c'
end

let rec conv lexp cont =
    match lexp with
    | Lam.Var v -> cont (Cps.Var v)
    | Lam.Int i -> cont (Cps.Int i)
    | Lam.String s -> cont (Cps.String s) (* didn't do the single-char optimization *)
    | Lam.Record fs -> conv_lst fs (fun vs -> 
        let b = Gensym.genvar () in
        Cps.Record (List.map (fun v -> (v, Cps.Offp 0)) vs, b, cont (Cps.Var b))
    )
    | Lam.Select (i, e) -> conv e (fun v -> 
        let b = Gensym.genvar () in
        Cps.Select (i, v, b, cont (Cps.Var b))
    )
    | _ -> raise (Invalid_argument "can't handle given lambda expression")

and conv_lst lexps cont =
    let rec conv_lst_rec lexps acc cont =
        match lexps with
        | [] -> cont (List.rev acc)
        | e :: es -> conv e (fun v -> conv_lst_rec es (v :: acc) cont)
    in
    conv_lst_rec lexps [] cont