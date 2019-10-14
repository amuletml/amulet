module A =
  let x = 0

  let y = 0

  open import "./not_found.ml"

  (* Won't error, as open will abandon things.

     TODO: Could we revisit this? Ideally we'd be able to continue resolving
     /some/ things, especially the other checks (such as non-linear patterns,
     etc...) *)
  let () = not_defined

module B = open import "./not_found.ml"
module C = B

module M =
  (* None of these will error, as their modules failed and so are skipped *)
  let () = (A.x, A.y, A.z)
  let () = B.x
  let () = C.x

  (* This should fail. *)
  let () = not_defined
