private module Base_defs = import {
  lua = "./base/lua.ml",
}

include Base_defs.Public

(* Boolean operators: *)
let a || b =
  if a then true else force b

let a && b =
  if a then force b else false

let not a = if a then false else true

(* Explicit type signatures for VTA: *)
let const (x : 'a) (_ : 'b) : 'a = x

(* The 'eq' class *)

class eq 'a begin
  val (==) : 'a -> 'a -> bool
  val (<>) : 'a -> 'a -> bool

  let x <> y = not (x == y)
  let x == y = not (x <> y)
end

instance eq int    begin let (==) = Base_defs.int_eq end
instance eq float  begin let (==) = Base_defs.float_eq end
instance eq string begin let (==) = Base_defs.string_eq end

instance eq () begin
  let _ == _ = true
  let _ <> _ = false
end

instance eq 'a => eq (list 'a) begin
  let xs == ys =
    match xs, ys with
    | Cons (x, xs), Cons (y, ys) ->
        if x == y then
          xs == ys
        else
          false
    | Nil, Nil -> true
    | _, _ -> false
end

instance eq 'a * eq 'b => eq ('a * 'b) begin
  let (a, b) == (c, d) = a == c && b == d
end

(* The 'ord' class *)

type ordering = Lt | Eq | Gt

class eq 'a => ord 'a begin
  val (<) : 'a -> 'a -> bool
  val (>) : 'a -> 'a -> bool
  val (<=) : 'a -> 'a -> bool
  val (>=) : 'a -> 'a -> bool
  val compare : 'a -> 'a -> ordering

  let compare x y =
    if x == y then
      Eq
    else if x <= y then
      Lt
    else
      Gt

  let x > y  = match compare x y with | Gt -> true  | _ -> false
  let x >= y = match compare x y with | Lt -> false | _ -> true
  let x < y  = match compare x y with | Lt -> true  | _ -> false
  let x <= y = match compare x y with | Gt -> false | _ -> true
end

instance eq ordering begin
  let a == b =
    match a, b with
    | Gt, Gt -> true
    | Eq, Eq -> true
    | Lt, Lt -> true
    | _, _ -> false
end

instance ord 'a * ord 'b => ord ('a * 'b) begin
  let (a, b) `compare` (c, d) =
    match a `compare` c with
    | Eq -> b `compare` d
    | Lt -> Lt
    | Gt -> Gt
end

instance ord () begin
  let _ < _ = false
  let _ > _ = false
  let _ >= _ = true
  let _ <= _ = true
  let _ `compare` _ = Eq
end

instance ord int begin
  let (<)  = Base_defs.lt_int
  let (<=) = Base_defs.lte_int
  let (>)  = Base_defs.gt_int
  let (>=) = Base_defs.gte_int
end

instance ord string begin
  let (<)  = Base_defs.lt_string
  let (<=) = Base_defs.lte_string
  let (>)  = Base_defs.gt_string
  let (>=) = Base_defs.gte_string
end

instance ord 'a => ord (list 'a) begin
  let compare =
    let loop xs ys =
      match xs, ys with
      | [], [] -> Eq
      | [], Cons _ -> Lt
      | Cons _, [] -> Gt
      | Cons (x, xs), Cons (y, ys) ->
          match x `compare` y with
          | Eq -> xs `compare` ys
          | Lt -> Lt
          | Gt -> Gt
    loop
end

(* The 3 "control" classes *)

class functor 'f begin
  val (<$>) : ('a -> 'b) -> 'f 'a -> 'f 'b
end

(* Bindings for idiom brackets *)
class functor 'f => applicative 'f begin
  val pure : forall 'a. 'a -> 'f 'a
  val (<*>) : 'f ('a -> 'b) -> 'f 'a -> 'f 'b
end

(* Bindings for monad syntax *)
class applicative 'f => monad 'f begin
  val (>>=) : 'f 'a -> ('a -> 'f 'b) -> 'f 'b
  val join  : 'f ('f 'a) -> 'f 'a

  let join x = begin
    with x <- x
    x
  end

  let m >>= f = join (f <$> m)
end

instance functor list begin
  let f <$> xs = [ f x | with x <- xs ]
end

instance applicative list begin
  let pure x = [x]
  let fs <*> xs = [ f x | with f <- fs, with x <- xs ]
end

instance monad list begin
  let xs >>= f = [ y | with x <- xs, with y <- f x ]
  let join xss = [ x | with xs <- xss, with x <- xs ]
end

(* The 'show' class *)

class show 'a begin
  val show : 'a -> string
end

instance show int begin
  let show = Base_defs.string_of_int
end

instance show float begin
  let show = Base_defs.string_of_float
end

instance show () begin
  let show () = "()"
end

instance show 'a => show (list 'a) begin
  let show =
    let a <> b =
      if a `Base_defs.string_eq` "" then
        show b
      else
        a ^ ", " ^ show b
    let rec loop acc = function
      | [] -> acc ^ "]"
      | Cons (x, xs) -> loop (acc <> x) xs
    fun x ->
      "[" ^ loop "" x
end

instance (show 'a * show 'b) => show ('a * 'b) begin
  let show (x, y) = "(" ^ show x ^ ", " ^ show y ^ ")"
end

instance show bool begin
  let show = function
    | true -> "true"
    | false -> "false"
end

instance show 'a => show (ref 'a) begin
  let show x = "ref (" ^ show (!x) ^ ")"
end

let negate x  = 0 - x
let negatef x = 0.0 -. x

(* List construction *)
let x :: xs = Cons (x, xs)
let cons x xs = x :: xs
