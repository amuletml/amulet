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

external val (<.)  : float -> float -> bool = "function (x, y) return x < y end"
external val (<=.) : float -> float -> bool = "function (x, y) return x <= y end"
external val (>.)  : float -> float -> bool = "function (x, y) return x > y end"
external val (>=.) : float -> float -> bool = "function (x, y) return x >= y end"

(* Boolean operators: *)
let a || b =
  if a then true else force b

let a && b =
  if a then force b else false

let not a = if a then false else true

(* Explicit type signatures for VTA: *)
let id (x : 'a) : 'a = x
let const (x : 'a) (_ : 'b) : 'a = x

(* The 'eq' class *)

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

external private val lt_int : int -> int -> bool = "function (x, y) return x < y end"
external private val lte_int : int -> int -> bool = "function (x, y) return x <= y end"
external private val gt_int : int -> int -> bool = "function (x, y) return x > y end"
external private val gte_int : int -> int -> bool = "function (x, y) return x >= y end"

instance ord () begin
  let _ < _ = false
  let _ > _ = false
  let _ >= _ = true
  let _ <= _ = true
  let _ `compare` _ = Eq
end

instance ord int begin
  let (<) = lt_int
  let (<=) = lte_int
  let (>) = gt_int
  let (>=) = gte_int
end

external private val lt_string : string -> string -> bool = "function (x, y) return x < y end"
external private val lte_string : string -> string -> bool = "function (x, y) return x <= y end"
external private val gt_string : string -> string -> bool = "function (x, y) return x > y end"
external private val gte_string : string -> string -> bool = "function (x, y) return x >= y end"

instance ord string begin
  let (<) = lt_string
  let (<=) = lte_string
  let (>) = gt_string
  let (>=) = gte_string
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

external private val string_of_int : int -> string = "tostring"
external private val string_of_float : float -> string = "tostring"

class show 'a begin
  val show : 'a -> string
end

instance show int begin
  let show = string_of_int
end

instance show float begin
  let show = string_of_float
end

instance show () begin
  let show () = "()"
end

instance show 'a => show (list 'a) begin
  let show =
    let a <> b =
      if a `string_eq` "" then
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

external val lua_version : string = "_VERSION"

let negate x  = 0 - x
let negatef x = 0.0 -. x

(* List construction *)
let x :: xs = Cons (x, xs)
let cons x xs = x :: xs
