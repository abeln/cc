open Core.Std

module Info = struct
    module Set = String.Set
    
    type t = {used: Set.t}

    let empty: t = {used = Set.empty}

    let mark_used ({used} as info) = function
    | Cps.Var v -> {used = Set.add used v}
    | _ -> info

    let used {used} var = Set.mem used var
end

let preproc exp =
    let rec proc acc = function
    | Cps.Record (fields, _, cont) -> (
        let acc' = List.fold fields ~init:acc ~f:(fun acc2 (vl, _) -> Info.mark_used acc2 vl) in
        proc acc' cont
    )
    | Cps.Select (_, vl, _, cont) -> proc (Info.mark_used acc vl) cont
    | Cps.Offset (_, vl, _, cont) -> proc (Info.mark_used acc vl) cont
    | Cps.App (fn, args) -> List.fold (fn :: args) ~init:acc ~f:(fun acc2 vl -> Info.mark_used acc2 vl)
    | Cps.Fix (fundefs, cont) -> (
        let acc' = List.fold fundefs ~init:acc ~f:(fun acc2 (_, _, body) -> proc acc2 body) in
        proc acc' cont
    )
    | Cps.Switch (vl, conts) -> (
        let acc' = Info.mark_used acc vl in
        List.fold conts ~init:acc' ~f:proc
    )
    | Cps.Primop (_, args, _, conts) -> (
        let acc' = List.fold args ~init:acc ~f:(fun acc2 vl -> Info.mark_used acc2 vl) in
        List.fold conts ~init:acc' ~f:proc
    )
    in
    proc Info.empty exp
    
let contract exp =
    let nclicks = ref 0 in
    let click () = nclicks := !nclicks + 1 in
    let pre = preproc exp in
    let rec go = function
    | Cps.Record (fields, v, cont) -> if (Info.used pre v) then Cps.Record (fields, v, go cont) else go cont
    | Cps.Select (off, vl, var, cont) -> if Info.used pre var then Cps.Select (off, vl, var, go cont) else go cont
    | Cps.Offset (off, vl, var, cont) -> Cps.Offset (off, vl, var, go cont)
    | Cps.App _ as fun_app -> fun_app
    | Cps.Fix (fundefs, cont) -> (
        let rdefs = List.fold fundefs ~init:[] ~f:(fun acc (name, args, body) ->
            if Info.used pre name then (name, args, go body) :: acc else acc)
        in
        if List.is_empty rdefs then go cont else Cps.Fix (List.rev rdefs, go cont)
    )
    | Cps.Primop (op, args, res, conts) -> Cps.Primop (op, args, res, List.map conts ~f:go)
    | Cps.Switch (vl, alts) -> Cps.Switch (vl, List.map alts ~f:go)
    in
    go exp