open Core.Std

module Set = String.Set
module Map = String.Map

module Info = struct
    (* The argument arity tracks whether an argument is always a record (in an application) and,
       if so, the number of fields in the record. *)
    type arg_arity =
        | AriNotSet
        | AriAlwaysRecord of int 
        | AriUnknown
        [@@deriving sexp_of]

    type def_info =
        | Record of int (* number of fields *)
        | Fun of Cps.var list * arg_arity list
        | FlatFun of (Cps.var * bool * Cps.var list) list 
        | ArgNotSet of Cps.var
        | ArgOnlySelect of Cps.var * int (* function name and highest-selected offset in all paths *)
        | ArgOther (* the argument is used not only in select nodes *)
        [@@deriving sexp_of]

    type t = {used: Set.t; escape: Set.t; def: def_info Map.t}

    let empty = {used = Set.empty; escape = Set.empty; def = Map.empty}

    let add_def {used; escape; def} var info = {used; escape; def = Map.add def ~key:var ~data:info}

    let add_escape {used; escape; def} var = {used; escape = Set.add escape var; def}

    let used info var = Set.mem info.used var

    let flat_args {def; _} fn arg = match Map.find def fn with
    | Some (FlatFun arg_descs) -> (
        let rec find = function
        | [] -> None
        | (arg2, should_flatten, new_args) :: rest -> (
            if arg2 = arg then (
                if should_flatten then Some new_args
                else None
            ) else find rest
        ) in
        find arg_descs
    )
    | _ -> None

    let rec init_arity n = match n with
    | 0 -> []
    | _ -> AriNotSet :: init_arity (n - 1)

    (* If the variable points to an argument that's only used in the body of a select, update the accessed offset. *)
    let bump_if_arg ({def; _} as info) var offset = match Map.find def var with
    | Some (ArgNotSet fname) -> add_def info var (ArgOnlySelect (fname, offset))
    | Some (ArgOnlySelect (fname, offset2)) -> add_def info var (ArgOnlySelect (fname, max offset offset2))
    | _ -> info

    (* If the variable points to an argument that's only used in the body of a select, mark it as not doing so anymore. *)
    let unmark_if_arg ({def; _} as info) var = match Map.find def var with
    | Some (ArgOnlySelect _) | Some (ArgNotSet _) -> add_def info var ArgOther
    | _ -> info

    let record_args_arity ({def; _} as info) fname args = match Map.find def fname with
    | Some (Fun (vars, arity)) -> (
        if List.length args <> List.length arity then raise (Invalid_argument (Printf.sprintf "expected %d arguments, but got %d in function %s" (List.length arity) (List.length args) fname))
        else (
            let arity_and_args = List.zip_exn arity args in
            let arity2 = List.map arity_and_args ~f:(fun (ari, arg) ->
                match arg with
                | Cps.Var v -> (
                    match (ari, Map.find def v) with
                    | (AriNotSet, Some (Record n)) -> AriAlwaysRecord n
                    | (AriNotSet, _) -> AriUnknown
                    | (AriAlwaysRecord n, Some (Record m)) -> if n = m then AriAlwaysRecord n else AriUnknown
                    | (AriAlwaysRecord _, _) -> AriUnknown
                    | (AriUnknown, _) -> AriUnknown
                )
                | _ -> AriUnknown
            ) in
            add_def info fname (Fun (vars, arity2))
        )
    )
    | _ -> info
    (* | _ -> raise (Invalid_argument (Printf.sprintf "expected a function called %s, but got something else" fname)) *)

    (* Register a use of a value as an argument of a given expression. *)
    let add_use ({used; escape; def} as info) vl exp = match vl with
    | Cps.Var v -> (
        let info2 = {used = Set.add used v; escape; def} in
        match exp with
        | Cps.Select (offset, _, _, _) -> bump_if_arg info2 v offset
        | _ -> (
            let info3 = unmark_if_arg info2 v in
            match exp with
            | Cps.Record (_, _, _) -> add_escape info3 v
            | _ -> info3
        )
    )
    | _ -> info

    let finalize {used; escape; def} =
        let keys = Map.keys def in
        let def2 = List.fold keys ~init:def ~f:(fun def3 k ->
            match Map.find def3 k with
            | Some (Fun (vars, ari)) -> (
                let vars_and_ari = List.zip_exn vars ari in
                let flat_args = List.map vars_and_ari ~f:(fun (v, a) ->
                    match (Map.find def3 v, a) with
                    | (Some (ArgOnlySelect (_, n)), AriAlwaysRecord m) -> (
                        if m >= n + 1 then (v, true, Gensym.gen_args (n + 1))
                        else (v, false, [v])
                    )
                    | _ -> (v, false, [v])
                ) in
                Map.add def3 ~key:k ~data:(FlatFun flat_args)
            )
            | _ -> def3
        ) in
        {used; escape; def = def2}
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
    | Cps.App (fn, args) -> (
        let acc3 = List.fold (fn :: args) ~init:acc ~f:(fun acc2 vl -> Info.add_use acc2 vl exp) in
        match fn with
        | Cps.Var fun_var -> Info.record_args_arity acc3 fun_var args
        | _ -> acc3
    )   
    | Cps.Fix (fundefs, cont) -> (
        let acc2 = List.fold fundefs ~init:acc ~f:(fun acc3 (fname, args, body) ->
            let acc4 = Info.add_def acc3 fname (Info.Fun (args, Info.init_arity (List.length args))) in
            let acc5 = List.fold args ~init:acc4 ~f:(fun acc6 arg -> Info.add_def acc6 arg (Info.ArgNotSet fname)) in
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

let indices n =
    let rec indices_acc n acc =
        match n with
        | 0 -> List.rev acc
        | _ -> indices_acc (n - 1) ((n - 1) :: acc)
    in
    indices_acc n []

let gen_select rec_val vars base =
    let vars_and_indices = List.zip_exn vars (indices (List.length vars)) in
    List.fold vars_and_indices ~init:base ~f:(fun acc (v, i) ->
        Cps.Select (i, rec_val, v, acc))

let unwrap_vars_exn vars = List.map vars ~f:(fun v -> match v with
    | Cps.Var v -> v
    | _ -> raise (Invalid_argument "expected a var, but got something else")
)

let contract exp =
    let nclicks = ref 0 in
    let click () = nclicks := !nclicks + 1 in

    let rec go_repeat exp =
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
            else (click (); go cont)
        | Cps.Offset (off, vl, var, cont) -> Cps.Offset (off, rename vl, var, go cont)
        | Cps.App (fn, args) -> (
            match fn with
            | Cps.Var fun_name -> (match Map.find info.Info.def fun_name with
                | Some (Info.FlatFun flat_args) ->
                    let flat_args_and_actual = List.zip_exn flat_args args in
                    let new_args = List.map flat_args_and_actual ~f:(fun ((_, is_flat, arg_list), arg) ->
                        if is_flat then (true, arg, Gensym.gen_val_res (List.length arg_list))
                        else (false, arg, [arg])) in
                    let new_args_raw = List.map new_args ~f:(fun (_, _, args) -> args) |> List.concat in
                    let base = Cps.App (rename fn, rename_multi new_args_raw) in
                    List.fold new_args ~init:base ~f:(fun acc (is_flat, arg_name, args) ->
                        if is_flat then (click (); gen_select arg_name (unwrap_vars_exn args) acc)
                        else acc
                    )
                | _ -> Cps.App (rename fn, rename_multi args)
            )
            | _ -> Cps.App (rename fn, rename_multi args)
        )
        | Cps.Fix (fundefs, cont) -> (
            let rdefs = List.fold fundefs ~init:[] ~f:(fun acc (name, args, body) ->
                if not (Info.used info name) then acc
                else (match Map.find info.Info.def name with
                    | Some (Info.FlatFun flat_args) ->
                        let all_args = List.concat (List.map flat_args ~f:(fun (_, is_flat, a) ->
                            if is_flat then click ();
                            a)) in
                        (name, all_args, go body) :: acc
                    | _ -> (name, args, go body) :: acc
                )
            )
            in
            if List.is_empty rdefs then (click (); go cont)
            else Cps.Fix (List.rev rdefs, go cont)
        )
        | Cps.Primop (Cps.Plus, [Cps.Int n; Cps.Int 0], [res], [cont]) -> (click (); new_name res (Cps.Int n); go cont)
        | Cps.Primop (Cps.Plus, [Cps.Int 0; Cps.Int n], [res], [cont]) -> (click (); new_name res (Cps.Int n); go cont)
        | Cps.Primop (op, args, res, conts) -> Cps.Primop (op, rename_multi args, res, List.map conts ~f:go)
        | Cps.Switch (vl, alts) -> Cps.Switch (rename vl, List.map alts ~f:go)
        | Cps.Select (index, vl, res, cont) -> (match vl with
            | Cps.Var var -> (
                match Map.find info.Info.def var with
                | Some (Info.ArgOnlySelect (fname, _)) -> (match Info.flat_args info fname var with
                    | Some new_args -> (
                        click ();
                        new_name res (Cps.Var (List.nth_exn new_args index));
                        go cont
                    )
                    | None -> Cps.Select (index, rename (Cps.Var var), res, go cont)
                )
                | _ -> Cps.Select (index, rename (Cps.Var var), res, go cont)
                )
            | _ -> Cps.Select (index, rename vl, res, go cont)
        )
        in

        let clicks = !nclicks in
        let res = go exp in
        if !nclicks > clicks then go_repeat res
        else res

    in
    go_repeat exp