let f x = f x
and g = f
(* g and f are in different binding groups!
 * CyclicSCC [f]
 * AcyclicSCC g
 *)
