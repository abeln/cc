open Core.Std;;

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

let rec gen_args n =
    if n = 0 then [] else new_arg () :: gen_args (n - 1)

let rec gen_res n =
    if n = 0 then [] else new_res () :: gen_res (n - 1)

let gen_val_res n = List.map (gen_res n) ~f:(fun v -> Cps.Var v)
