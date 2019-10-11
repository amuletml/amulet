external val ( + )  : int -> int -> int   = "function (x, y) return x + y end"
external val ( * )  : int -> int -> int   = "function (x, y) return x * y end"
external val ( - )  : int -> int -> int   = "function (x, y) return x - y end"
external val ( ** ) : int -> int -> int   = "function (x, y) return x ^ y end"
external val ( / )  : int -> int -> float = "function (x, y) return x / y end"

external val ( +. )  : float -> float -> float = "function (x, y) return x + y end"
external val ( *. )  : float -> float -> float = "function (x, y) return x * y end"
external val ( -. )  : float -> float -> float = "function (x, y) return x - y end"
external val ( **. ) : float -> float -> float = "function (x, y) return x ^ y end"
external val ( /. )  : float -> float -> float = "function (x, y) return x / y end"

external val (^) : string -> string -> string = "function(x, y) return x .. y end"

external val (<) : int -> int -> bool = "function (x, y) return x < y end"
external val (<=) : int -> int -> bool = "function (x, y) return x <= y end"
external val (>) : int -> int -> bool = "function (x, y) return x > y end"
external val (>=) : int -> int -> bool = "function (x, y) return x >= y end"

external val (<.)  : float -> float -> bool = "function (x, y) return x < y end"
external val (<=.) : float -> float -> bool = "function (x, y) return x <= y end"
external val (>.)  : float -> float -> bool = "function (x, y) return x > y end"
external val (>=.) : float -> float -> bool = "function (x, y) return x >= y end"

(* Boolean operators: *)
let a || b =
  if a then true else b

let a && b =
  if a then b else false

let not a = if a then false else true

class eq 'a begin
  val (==) : 'a -> 'a -> bool
  val (<>) : 'a -> 'a -> bool

  let x <> y = not (x == y)
  let x == y = not (x <> y)
end

external private val int_eq    : int -> int -> bool       = "function(x, y) return x == y end"
external private val float_eq  : float -> float -> bool   = "function(x, y) return x == y end"
external private val string_eq : string -> string -> bool = "function(x, y) return x == y end"

instance eq int    begin let (==) = int_eq end
instance eq float  begin let (==) = float_eq end
instance eq string begin let (==) = string_eq end

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

external val float_of_int : int -> float = "function(x) return x end"
external val int_of_float : float -> int =
  "function(x) \
    if x >= 0 then \
      return math.floor(x + 0.5) \
    else \
      return math.ceil(x - 0.5) \
    end \
   end"

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
