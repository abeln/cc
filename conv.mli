
val to_cps : Lam.lexp -> (Cps.value -> Cps.cexp) -> Cps.cexp

val to_cps_lst : Lam.lexp list -> (Cps.value list -> Cps.cexp) -> Cps.cexp
