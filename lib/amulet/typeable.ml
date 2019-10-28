open import "./base.ml"
open import "./option.ml"

type 'a :~: 'b =
  Refl : 'a ~ 'b => 'a :~: 'b

deriving instance typeable (:~:)

type proxy 'a = Proxy

deriving instance typeable proxy

let is_same_type (p : proxy 'a) (q : proxy 'b) : option ('a :~: 'b) =
  eq_type_rep (type_of p) (type_of q)
    (fun _ -> Some (Refl : 'a :~: 'b))
    (fun _ -> None)

let cast
  :
    forall 'a 'b. typeable 'a * typeable 'b => 'a -> option 'b
  =
    fun x ->
      eq_type_rep
        (type_of (Proxy @'a))
        (type_of (Proxy @'b))
        (fun _ -> Some x)
        (fun _ -> None)

(** An opaque box that carries typeable evidence *)
type dynamic =
  Dynamic : forall 'a. typeable 'a => 'a -> dynamic

deriving instance typeable dynamic

let from_dynamic (Dynamic x) = cast x
