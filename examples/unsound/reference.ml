type option 'a = None | Some of 'a
type ref 'a =
  | Ref of 'a
  | NotANewtype (* oh my compiler, how silly you are *)


let ref = Ref
and read (Ref x) = x

external val upd : ref 'a -> 'a -> ref 'a =
  "function(ref, x) ref[2.0] = x; return ref end"
external val print : string -> unit = "print"

(* inferred type: ∀ α. ref (option α) *)
let r = ref None

let () =
  upd r (Some 1)
  match read r with
  | Some x -> print (x ())

(* This program is well-behaved but crashes at runtime with a Lua error,
 * thus proving that the Amulet type system is unsound, to some extent.
 * Not a happy result.
 *
 * However, since my speciality is rationalising things away, this too
 * can be rationalised, by using the same "sound up to FFI" argument as
 * many other languages. For instance:
 *)

external val coerce : 'a -> 'b = "function(x) return x end"

let () = print ((coerce 1) ())

(* Nothing we can do to stop this. Since mutation is not a language
 * feature, I am not introducing any ugly workarounds into the type
 * system. *)
