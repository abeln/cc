
val conv : Lam.lexp -> (Cps.value -> Cps.cexp) -> Cps.cexp

val conv_lst : Lam.lexp list -> (Cps.value list -> Cps.cexp) -> Cps.cexp
