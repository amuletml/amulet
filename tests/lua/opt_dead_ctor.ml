type wrapper 'm = Wrapper of (forall 'a. 'a -> 'a)
let monad_option = Wrapper (fun f -> f) : wrapper int
