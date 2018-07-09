open Core.Std

let rec extract = function
| Cps.Record (fields, res, cont) ->
    let (cont', defs) = extract cont in
    (Cps.Record (fields, res, cont'), defs)
| Cps.Select (off, record, res, cont) ->
    let (cont', defs) = extract cont in
    (Cps.Select (off, record, res, cont'), defs)
| Cps.Offset (off, record, res, cont) ->
    let (cont', defs) = extract cont in
    (Cps.Offset (off, record, res, cont'), defs)
| Cps.App _ as app -> (app, [])
| Cps.Fix (fundefs, body) ->
    let bodies = List.map fundefs ~f:(fun (_, _, body) -> body) in
    let (bodies', defs) = extract_multi bodies in
    let fundefs' = List.map (List.zip_exn fundefs bodies') ~f:(fun ((fname, args, _), new_body) -> (fname, args, new_body)) in
    (body, List.append fundefs' defs)
| Cps.Switch (vl, conts) ->
    let (conts', defs) = extract_multi conts in
    (Cps.Switch (vl, conts'), defs)
| Cps.Primop (op, args, res, conts) -> 
    let (conts', defs) = extract_multi conts in
    (Cps.Primop (op, args, res, conts'), defs)

and extract_multi conts = 
    let (conts', fundefs) = List.fold conts ~init:([], []) ~f:(fun (acc_cont, acc_defs) cont ->
        let (cont', defs) = extract cont in
        (cont' :: acc_cont, defs :: acc_defs)
    ) in
    (List.rev conts', List.concat fundefs |> List.rev)

let lift exp = 
    let (exp', fundefs) = extract exp in
    match fundefs with
    | [] -> exp'
    | _ -> Cps.Fix (fundefs, exp')
    