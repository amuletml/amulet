external val error : string -> 'a = "error"

type nat = Z | S of nat

type 'a × 'b <- 'a * 'b

type vect 'a 'n =
  | Nil_  : vect 'a Z
  | Cons_ : 'a × vect 'a 'n -> vect 'a (S 'n)

type fin 'n =
  | FZ : fin (S 'n)
  | FS : fin 'n -> fin (S 'n)

let rec f (vect : vect 'a 'n) (idx : fin 'n) : 'a =
  match vect, idx with
  | Nil_, _ -> error ""
  | Cons_ (x, _), FZ -> x
  | Cons_ (_, xs), FS k -> f xs k

let rec g (vect : vect 'a 'n) (idx : fin 'n) : 'a =
  match vect, idx with
  | Cons_ (x, _), FZ -> x
  | Cons_ (_, xs), FS k -> g xs k
