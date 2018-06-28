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
