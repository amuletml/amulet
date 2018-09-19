let bind
  : forall 'm 'row.
    { 'row | bind : forall 'a 'a. ('a -> 'm 'a) -> 'a -> 'm 'a }
    -> forall 'a. ('a -> 'm 'a) -> 'a -> 'm 'a
  = fun r -> r.bind

let pure
  : forall 'm 'row.
    { 'row | pure : forall 'a. 'a -> 'm 'a }
    -> forall 'a. 'a -> 'm 'a
  = fun r -> r.pure

let foo a x = bind a (pure a) x
