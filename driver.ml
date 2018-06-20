open Core.Std

let c v =
    Cps.App (Cps.Var "id", [v])

let l = Lam.Record [Lam.Int 4; Lam.Select(0, Lam.Var "m")]

let cps_l = Conv.conv l c

let () = (
    printf "%s\n" (Lam.lexp_to_string l);
    printf "\n";
    printf "%s\n" (Cps.cexp_to_string cps_l)
)