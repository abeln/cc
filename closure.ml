open Core.Std

module Set = String.Set
module Map = String.Map

let singleton v = Set.add Set.empty v

let rec free_vars exp = 
    let unwrap = function
    | Cps.Var v -> singleton v
    | _ -> Set.empty
    in
    let collect exps = List.fold exps ~init:Set.empty ~f:(fun acc exp -> Set.union acc (free_vars exp)) in
    let collect_vals vals = List.fold vals ~init:Set.empty ~f:(fun acc v -> Set.union acc (unwrap v)) in
    match exp with
    | Cps.Record (fields, res, cont) ->
        let field_vars = List.map fields ~f:(fun (v, _) -> v) |> collect_vals in
        Set.remove (Set.union field_vars (free_vars cont)) res
    | Cps.Select (_, record, res, cont) -> Set.remove (Set.union (free_vars cont) (unwrap record)) res
    | Cps.Offset (_, record, res, cont) -> Set.remove (Set.union (free_vars cont) (unwrap record)) res
    | Cps.App (f, args) -> collect_vals (f :: args)
    | Cps.Fix (fundefs, cont) ->
        let in_defs = List.fold fundefs ~init:Set.empty ~f:(fun acc (fname, args, body) -> Set.union acc (Set.diff (free_vars body) (Set.of_list (fname :: args)))) in
        let fun_names = List.map fundefs ~f:(fun (name, _, _) -> name) |> Set.of_list in
        Set.diff (Set.union in_defs (free_vars cont)) fun_names
    | Cps.Switch (_, conts) -> collect conts
    | Cps.Primop (_, args, res, conts) ->
        let in_conts = collect conts in
        let minus_res = Set.diff in_conts (Set.of_list res) in
        Set.union (collect_vals args) minus_res

let rec subst exp sub_map =
    let go exp = subst exp sub_map in
    let sub_val v = match v with
    | Cps.Var v' -> (match Map.find sub_map v' with
        | Some v'' -> Cps.Var v''
        | None -> v
    )
    | _ -> v
    in
    match exp with
    | Cps.Record (fields, res, cont) ->
        let fields' = List.map fields ~f:(fun (vl, path) -> (sub_val vl, path)) in
        Cps.Record (fields', res, go cont)
    | Cps.Select (off, vl, res, cont) -> Cps.Select (off, sub_val vl, res, go cont)
    | Cps.Offset (off, vl, res, cont) -> Cps.Select (off, sub_val vl, res, go cont)
    | Cps.App (f, args) -> Cps.App (sub_val f, List.map args ~f:sub_val)
    | Cps.Fix (fundefs, body) ->
        let fundefs' = List.map fundefs ~f:(fun (fname, args, cont) -> (fname, args, go cont)) in
        Cps.Fix (fundefs', go body)
    | Cps.Switch (vl, conts) -> Cps.Switch (sub_val vl, List.map conts ~f:go)
    | Cps.Primop (op, args, res, conts) -> Cps.Primop (op, List.map args ~f:sub_val, res, List.map conts ~f:go)

type fundef = Cps.var * Cps.var list * Cps.cexp

type cls_info = Cps.var list * (int Map.t)

let offset_map vars = List.foldi vars ~init:Map.empty ~f:(fun offset acc v -> Map.add acc ~key:v ~data:offset)

let offset off_map base v = match (Map.find off_map base, Map.find off_map v) with
| (Some off_base, Some off_v) -> off_v - off_base
| _ -> raise (Invalid_argument (Printf.sprintf "can't find either %s or %s in closure map" base v))

let close (fundefs: fundef list) : (fundef list) * cls_info =
    let fun_names = List.map fundefs ~f:(fun (fname, _, _) -> fname) in
    let is_fun v = List.mem fun_names v in
    let close_one (fname, args, body) cls_map fvars =
        let cls_arg = Gensym.new_clo () in
        let sub_map = List.fold fvars ~init:Map.empty ~f:(fun smap fv ->
            let fv' = if is_fun fv then Gensym.new_clo () else Gensym.new_arg () in
            Map.add smap ~key:fv ~data:fv')
        in
        let body_init = subst body sub_map in
        let body_full = List.fold fvars ~init:body_init ~f:(fun acc fv ->
            if is_fun fv then Cps.Offset (offset cls_map fname fv, Cps.Var cls_arg, Map.find_exn sub_map fv, acc)
            else Cps.Select (offset cls_map fname fv, Cps.Var cls_arg, Map.find_exn sub_map fv, acc)
        ) in 
        (fname, cls_arg :: args, body_full)
    in 
    let free_in_defs = List.map fundefs ~f:(fun (_, args, body) -> Set.diff (free_vars body) (Set.of_list args)) in
    let all_free = List.fold free_in_defs ~init:Set.empty ~f:(fun acc free_set -> Set.union acc free_set) in
    let all_minus_funs = Set.diff all_free (Set.of_list fun_names) in
    let free_list = List.append fun_names (Set.to_list all_minus_funs) in
    let off_map = offset_map free_list in
    let fundefs' = List.map (List.zip_exn fundefs free_in_defs) ~f:(fun (fdef, fvs) -> close_one fdef off_map (Set.to_list fvs)) in
    (fundefs', (free_list, off_map))

let rec convert = function
| Cps.Record (fields, res, cont) -> Cps.Record (fields, res, convert cont)
| Cps.Select (off, record, res, cont) -> Cps.Select (off, record, res, convert cont)
| Cps.Offset (off, record, res, cont) -> Cps.Offset (off, record, res, convert cont)
| Cps.App (f, args) ->
    let f' = Gensym.new_fun () in
    Cps.Select (0, f, f', Cps.App (Cps.Var f', f :: args))
| Cps.Fix (fundefs, body) ->
    let (fundefs', (free_list, off_map)) = close fundefs in
    let fundefs'' = List.map fundefs' ~f:(fun (fname, args, cont) -> (fname, args, convert cont)) in
    let fun_names = List.map fundefs ~f:(fun (fname, _, _) -> fname) in
    let sub_map = List.fold fun_names ~init:Map.empty ~f:(fun acc fname -> Map.add acc ~key:fname ~data:(Gensym.new_clo ())) in
    let body' = subst body sub_map in
    let body'' = convert body' in
    let rec_var = Gensym.new_clo () in
    let with_fun_offsets = List.fold fun_names ~init:body'' ~f:(fun acc fname ->
        Cps.Offset (Map.find_exn off_map fname, Cps.Var rec_var, Map.find_exn sub_map fname, acc)
    ) in
    let with_closure_record = Cps.Record (List.map free_list ~f:(fun fv -> (Cps.Var fv, Cps.Offp 0)), rec_var, with_fun_offsets) in
    Cps.Fix (fundefs'', with_closure_record)
| Cps.Switch (vl, conts) -> Cps.Switch (vl, List.map conts ~f:convert)
| Cps.Primop (op, args, res, conts) -> Cps.Primop (op, args, res, List.map conts ~f:convert)