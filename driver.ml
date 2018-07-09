open Core.Std

let id v =
    Cps.App (Cps.Var "id", [v])

let compile e = Conv.to_cps e id |> Contract.contract

let l = Lam.Record [
    Lam.App (
        (Lam.Prim Lam.Neg),
        Lam.App (
            (Lam.Prim Lam.Plus),
            Lam.Record [Lam.Int 4; Lam.Int 5])
    );
    Lam.Select(0, Lam.Var "m")]

let cps_l = Conv.to_cps l id

let l2 = Lam.App (Lam.Fn ("x", Lam.App (Lam.Prim Lam.Times, Lam.Record [Lam.Var "x"; Lam.Int 10])), Lam.Int 4)

let cps_l2 = Conv.to_cps l2 id

let fact = Lam.Fix (["fact"],
    [Lam.Fn ("n", Lam.App (Lam.Prim Lam.Times,
                           Lam.Record [Lam.Var "n"; Lam.App (Lam.Var "fact",
                                                             Lam.App (Lam.Prim Lam.Minus,
                                                                      Lam.Record [Lam.Var "n"; Lam.Int 1]))]))],
        Lam.App (Lam.Var "fact", Lam.Int 10))

let cps_fact = Conv.to_cps fact id

let if_exp = Lam.If (
    Lam.App (Lam.Prim Lam.Gt, Lam.Record [Lam.Int 2; Lam.Int 1]),
    (Lam.App (Lam.Prim Lam.Plus, Lam.Record [Lam.Int 40; Lam.Int 0])),
    (Lam.Int 50))

let if_exp_cexp = Conv.to_cps if_exp id

let unused = Cps.Record ([(Cps.Int 1, Cps.Offp 0); (Cps.Int 2, Cps.Offp 0)], "res", id (Cps.Var "foo"))

let unused_contract = Contract.contract unused

let flatten = Lam.Fix (["first"], [Lam.Fn ("rec", Lam.Select (1, Lam.Var "rec"))], Lam.App (Lam.Var "first", Lam.Record [Lam.Int 1; Lam.Int 2]))

let flatten_not_opt = Conv.to_cps flatten id

let flatten_cps = compile flatten

let closure_lam = Lam.Fn ("x", Lam.Fn ("y", Lam.App (Lam.Prim Lam.Plus, Lam.Record [Lam.Var "x"; Lam.Var "y"])))

let closure_cps = Conv.to_cps closure_lam id

let closure_cps_closure = Closure.convert closure_cps |> Lift.lift

let () = (
    printf !"%{sexp:Lam.lexp}\n" closure_lam;
    printf "\n";
    printf !"%{sexp:Cps.cexp}\n" closure_cps;
    printf "\n";
    printf !"%{sexp:Cps.cexp}\n" closure_cps_closure
)