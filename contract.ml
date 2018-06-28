open Core.Std

module Set = String.Set
module Map = String.Map

module Info = struct
    type def_info =
        | Record of int (* record arity *)
        (* Both flattable constructors store the highest selected index seen so far.
           A fluid flattable is one where the index can increase. In the fixed one,
           it can't. *)
        | FixedFlattable of int
        | FluidFlattable of int
        | Flattable of int * Cps.var list  
        | Arg of Cps.var
        [@@deriving sexp_of]

    type t = {used: Set.t; escape: Set.t; def: def_info Map.t}

    let empty = {used = Set.empty; escape = Set.empty; def = Map.empty}

    let add_def {used; escape; def} var info = {used; escape; def = Map.add def ~key:var ~data:info}

    let remove_if_flattable ({used; escape; def} as info) fname = match Map.find def fname with
    | Some (FixedFlattable _) | Some (FluidFlattable _) -> {used; escape; def = Map.remove def fname}
    | _ -> info

    let bump_if_flattable ({used; escape; def} as info) var n = match Map.find def var with
    | Some (Arg fname) -> (
        match Map.find def fname with
        | Some (FluidFlattable m) -> {used; escape; def = Map.add def ~key:var ~data:(FluidFlattable (max n m))}
        (* If we see a fixed flattable and we were supposed to bump it, remove it. *)
        | Some (FixedFlattable m) -> if m = n then info else remove_if_flattable info fname
        | _ -> info
    )
    | _ -> info

    let freeze_if_flattable ({used; escape; def} as info) fname n = match Map.find def fname with
    | Some (FixedFlattable m) -> if m = n then info else remove_if_flattable info fname
    | Some (FluidFlattable m) -> if n >= m then add_def info fname (FixedFlattable n) else remove_if_flattable info fname
    | _ -> info

    let add_escape {used; escape; def} var = {used; escape = Set.add escape var; def}

    (* Register a use of a value as an argument of a given expression. *)
    let add_use ({used; escape; def} as info) vl exp = match vl with
    | Cps.Var v -> (
        let info2 = {used = Set.add used v; escape; def} in
        match exp with
        | Cps.Select (index, _, _, _) -> bump_if_flattable info2 v index
        | _ -> (
            let info3 = remove_if_flattable info2 v in
            match exp with
            | Cps.Record (_, _, _) -> add_escape info3 v
            | Cps.App (Cps.Var fname, args) -> (
                if List.mem args (Cps.Var v) then match Map.find info3.def v with
                | Some (Record n) -> freeze_if_flattable info3 fname n 
                | _ -> info3
                else info3
            )
            | _ -> info3
        )
    )
    | _ -> info

    let rec gen_args n =
        if n = 0 then [] else Gensym.new_arg () :: gen_args (n - 1)

    let finalize {used; escape; def} =
        let keys = Map.keys def in
        let def2 = List.fold keys ~init:def ~f:(fun def3 k ->
            match Map.find def3 k with
            | Some (FixedFlattable n) | Some (FluidFlattable n) -> Map.add def3 ~key:k ~data:(Flattable (n, gen_args n))
            | _ -> def3
        ) in
        {used; escape; def = def2}

    let used info var = Set.mem info.used var
end

let preproc exp =
    let rec proc acc exp = match exp with
    | Cps.Record (fields, var, cont) -> (
        let acc2 = List.fold fields ~init:acc ~f:(fun acc' (vl, _) -> Info.add_use acc' vl exp) in
        let acc3 = Info.add_def acc2 var (Info.Record (List.length fields)) in
        proc acc3 cont
    )
    | Cps.Select (_, vl, _, cont) -> proc (Info.add_use acc vl exp) cont
    | Cps.Offset (_, vl, _, cont) -> proc (Info.add_use acc vl exp) cont
    | Cps.App (fn, args) -> List.fold (fn :: args) ~init:acc ~f:(fun acc2 vl -> Info.add_use acc2 vl exp)
    | Cps.Fix (fundefs, cont) -> (
        let acc2 = List.fold fundefs ~init:acc ~f:(fun acc3 (fname, args, body) ->
            let acc4 = Info.add_def acc3 fname (Info.FluidFlattable 0) in
            let acc5 = List.fold args ~init:acc4 ~f:(fun acc6 arg -> Info.add_def acc6 arg (Info.Arg fname)) in
            proc acc5 body
        ) in
        proc acc2 cont
    )
    | Cps.Switch (vl, conts) -> (
        let acc2 = Info.add_use acc vl exp in
        List.fold conts ~init:acc2 ~f:proc
    )
    | Cps.Primop (_, args, _, conts) -> (
        let acc2 = List.fold args ~init:acc ~f:(fun acc2 vl -> Info.add_use acc2 vl exp) in
        List.fold conts ~init:acc2 ~f:proc
    )
    in
    proc Info.empty exp |> Info.finalize

let contract exp =
    let nclicks = ref 0 in
    let click () = nclicks := !nclicks + 1 in
    let info: Info.t = preproc exp in
    let rename_map = ref Map.empty in
    let new_name var new_val = (rename_map := Map.add !rename_map ~key:var ~data:new_val) in
    let rename vl = match vl with
    | Cps.Var var -> (match Map.find !rename_map var with
                        | Some new_val -> new_val
                        | None -> Cps.Var var)
    | _ -> vl
    in
    let rename_multi l = List.map l ~f:rename in
    let rec go = function
    | Cps.Record (fields, v, cont) ->
        if (Info.used info v) then Cps.Record (List.map fields ~f:(fun (vl, path) -> (rename vl, path)), v, go cont)
        else go cont
    | Cps.Offset (off, vl, var, cont) -> Cps.Offset (off, rename vl, var, go cont)
    (* TODO: unwrap arguments to flattable functions *)
    | Cps.App (fn, args) -> Cps.App (rename fn, rename_multi args)
    | Cps.Fix (fundefs, cont) -> (
        let rdefs = List.fold fundefs ~init:[] ~f:(fun acc (name, args, body) ->
            if not (Info.used info name) then acc
            else (match Map.find info.Info.def name with
                | Some (Info.Flattable (n, newargs)) -> (name, newargs, go body) :: acc
                | _ -> (name, args, go body) :: acc
            )
        )
        in
        if List.is_empty rdefs then go cont
        else Cps.Fix (List.rev rdefs, go cont)
    )
    | Cps.Primop (Cps.Plus, [Cps.Int n; Cps.Int 0], [res], [cont]) -> (new_name res (Cps.Int n); go cont)
    | Cps.Primop (Cps.Plus, [Cps.Int 0; Cps.Int n], [res], [cont]) -> (new_name res (Cps.Int n); go cont)
    | Cps.Primop (op, args, res, conts) -> Cps.Primop (op, rename_multi args, res, List.map conts ~f:go)
    | Cps.Switch (vl, alts) -> Cps.Switch (rename vl, List.map alts ~f:go)
    | Cps.Select (index, vl, res, cont) -> (match vl with
        | Cps.Var var -> (
            match Map.find info.Info.def var with
            | Some (Info.Arg fname) -> (match Map.find info.Info.def fname with
                | Some (Info.Flattable (n, args)) -> (new_name res (Cps.Var (List.nth_exn args index)); go cont)
                | _ -> Cps.Select (index, rename (Cps.Var var), res, go cont)
            )
            | _ -> Cps.Select (index, rename (Cps.Var var), res, go cont)
            )
        | _ -> Cps.Select (index, rename vl, res, go cont)
    )
    in
    go exp