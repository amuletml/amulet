type ordering = Lt | Gt | Eq

type ord 'a = Ord of 'a -> 'a -> ordering

let implicit ord_int =
  let k x y =
    if x < y then
      Lt
    else if x > y then
      Gt
    else Eq
  Ord k

type eq 'a = MkEq of 'a -> 'a -> bool

let equals : forall 'a. eq 'a => 'a -> 'a -> bool = fun (MkEq f) -> f

let implicit eq_from_ord : forall 'a. ord 'a => eq 'a =
  fun (Ord k) -> MkEq (fun x y -> match k x y with | Eq -> true | _ -> false)

let main = equals : int -> int -> bool
