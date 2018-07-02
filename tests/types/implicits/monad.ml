type monad 'm = Monad of (forall 'a. 'a -> 'm 'a) * (forall 'a 'b. 'm 'a -> ('a -> 'm 'b) -> 'm 'b)

type maybe 'a = Just of 'a | Nothing

let implicit monad_maybe =
  let bind x f =
    match x with
    | Just m -> f m
    | Nothing -> Nothing
  Monad (Just, bind)

let pure ?(Monad (f, _)) = f

let (>>=) ?(Monad (_, f)) = f

let (<$>) ?m fn vl = vl >>= fun x -> pure (fn x)

let (>>) ?m x y = x >>= fun _ -> y

let main = pure 1 >> Nothing
