open import "%prelude"

type option 'a = Some of 'a | None

let from_option x = function
  | Some x -> x
  | None -> x

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
