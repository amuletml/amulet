(* The conditions where this optimisation will trigger are currently
   rather small, so we create a rather artificial test case here

    - The definition must be used multiple times (otherwise pre-inline
      unconditionally will kick in)

    - The definition must be local (otherwise the inliner is not smart
      enough to look through everything.
*)

external val ignore : 'a -> () = "nil"
let () =
  let semigroup_unit = { append = fun _ () -> () }
  ignore ( semigroup_unit.append () ()
         , semigroup_unit.append () () )
