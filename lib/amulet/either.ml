open import "./base.ml"

type either 'a 'b = Left of 'a | Right of 'b

let from_right x = function
  | Left _ -> x
  | Right b -> b

let from_left x = function
  | Left a -> a
  | Right _ -> x

instance (eq 'a * eq 'b) => eq (either 'a 'b) begin
  let x == y =
    match x, y with
    | Left a, Left b -> a == b
    | Right a, Right b -> a == b
    | _, _ -> false
end

instance functor (either 'a) begin
  let f <$> x =
    match x with
    | Left a -> Left a
    | Right b -> Right (f b)
end

instance applicative (either 'a) begin
  let pure = Right
  let f <*> x =
    match f, x with
    | Right f, r -> f <$> r
    | Left f, _ -> Left f
end

instance monad (either 'a) begin
  let x >>= f =
    match x with
    | Right x -> f x
    | Left x -> Left x
end

instance (show 'a * show 'b) => show (either 'a 'b) begin
  let show = function
    | Left a -> "Left (" ^ show a ^ ")"
    | Right b -> "Right (" ^ show b ^ ")"
end
