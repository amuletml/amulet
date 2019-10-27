open import "./base.ml"

type option 'a = Some of 'a | None

deriving typeable option

let from_option x = function
  | Some x -> x
  | None -> x

instance eq 'a => eq (option 'a) begin
  let x == y =
    match x, y with
    | None, None -> true
    | Some x, Some y -> x == y
    | _, _ -> false
end

instance functor option begin
  let f <$> x =
    match x with
    | None -> None
    | Some x -> Some (f x)
end

instance applicative option begin
  let pure = Some
  let f <*> x =
    match f, x with
    | Some f, Some x -> Some (f x)
    | _, _ -> None
end

instance monad option begin
  let xs >>= f =
    match xs with
    | Some x -> f x
    | None -> None
end

instance show 'a => show (option 'a) begin
  let show = function
    | None -> "None"
    | Some x -> "Some (" ^ show x ^ ")"
end
