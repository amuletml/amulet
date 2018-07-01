type monad 'm = Monad of (forall 'a. 'a -> 'm 'a) * (forall 'a 'b. 'm 'a -> ('a -> 'm 'b) -> 'm 'b)

type maybe 'a = Just of 'a | Nothing

let bind x f =
  match x with
  | Just m -> f m
  | Nothing -> Nothing
and implicit monad_maybe =
  Monad (Just, bind)

let pure : forall 'm 'a. monad 'm => 'a -> 'm 'a =
  fun (Monad (f, _)) -> f

let (>>=) : forall 'm 'a 'b. monad 'm => 'm 'a -> ('a -> 'm 'b) -> 'm 'b =
  fun (Monad (_, f)) -> f

let (<$>) : forall 'm 'a 'b. monad 'm => ('a -> 'b) -> 'm 'a -> 'm 'b =
  fun _ fn vl -> vl >>= fun x -> pure (fn x)

let (>>) : forall 'm 'a 'b. monad 'm => 'm 'a -> 'm 'b -> 'm 'b =
  fun _ x y -> x >>= fun _ -> y

let main = pure 1 >> Nothing
