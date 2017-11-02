type hide
  = Hide : 'a -> hide ;

let unhide (Hide x) = x ;

external val print : 'a -> unit = "print" ;

let main _ =
  let i = unhide (Hide ())
   in print (i + 1)

(* Explanation:
 * This *really* shouldn't happen. We oughtta restrict the scope of 'a in
 * hide's definition, otherwise we're allowed to do bad things to the type
 * system. This program serves as a proof of sneaky^W unsoundness in the type
 * system.
 *)
