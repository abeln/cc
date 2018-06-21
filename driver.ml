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

let () = (
    printf !"%{sexp:Lam.lexp}\n" l;
    printf "\n";
    printf !"%{sexp:Cps.cexp}\n" cps_l
)