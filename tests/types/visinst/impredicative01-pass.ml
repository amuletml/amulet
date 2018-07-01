type list 'a =
  | Nil : forall ('a : type) -> list 'a
  | Cons : forall ('a : type) -> 'a * list 'a -> list 'a

let ids = Cons @{forall 'a. 'a -> 'a} (fun x -> x, Nil @{forall 'a. 'a -> 'a})

let head : forall ('a : type) -> list 'a -> 'a = fun (Cons (a, _)) -> a

let main = head @{forall 'a. 'a -> 'a} ids 1
