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

let () = (
    printf !"%{sexp:Lam.lexp}\n" l2;
    printf "\n";
    printf !"%{sexp:Cps.cexp}\n" cps_l2
)