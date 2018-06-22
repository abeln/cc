open Core.Std

let id v =
    Cps.App (Cps.Var "id", [v])

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

let () = (
    printf !"%{sexp:Lam.lexp}\n" fact;
    printf "\n";
    printf !"%{sexp:Cps.cexp}\n" cps_fact
)