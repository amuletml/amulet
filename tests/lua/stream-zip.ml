external val ( + ) : int -> int -> int = "function(x, y) return x + y end"
external val ( * ) : int -> int -> int = "function(x, y) return x * y end"
external val ( ^ ) : string -> string -> string = "function(x, y) return x .. y end"
external val ( > ) : int -> int -> bool = "function(x, y) return x > y end"
external val ( <= ) : int -> int -> bool = "function(x, y) return x > y end"
external val io_write : string -> unit = "io.write"
external val print : string -> unit = "print"
external val to_string : 'a -> string = "tostring"
external val rem : int -> int -> int = "function(x, y) return x % y end"

type step 's 'a =
  | Skip of 's
  | Yield of 'a * 's
  | Done

type stream 'a =
  | Stream : forall 's. ('s -> step 's 'a) * 's -> stream 'a

type option 'a = Some of 'a | None

let take len (Stream (f, start)) =
  let go (i, st) =
    if i <= len then
      match f st with
      | Done -> Done
      | Skip s -> Skip (i + 1, s)
      | Yield (a, s) -> Yield (a, i + 1, s)
    else
      Done
  Stream (go, 1, start)

let map f (Stream (go, start)) =
  let loop st =
    match go st with
    | Done -> Done
    | Skip s -> Skip s
    | Yield (x, s) -> Yield (f x, s)
  Stream (loop, start)

let filter p (Stream (f, start)) =
  let go st =
    match f st with
    | Done -> Done
    | Skip s -> Skip s
    | Yield (a, s) ->
      if p a then
        Yield (a, s)
      else
        Skip s
  Stream (go, start)

let range (start, limit) =
  let go n =
    if n > limit then
      Done
    else
      Yield (n, n + 1)
  Stream (go, start)

let fold_stream f z (Stream (stream, start)) =
  let rec go ac st =
    match stream st with
    | Yield (a, st) -> go (f a ac) st
    | Skip st -> go ac st
    | Done -> ac
  go z start

let dump_stream e (Stream (f, start)) =
  let rec go st =
    match f st with
    | Skip st -> go st
    | Yield (a, st) ->
      io_write ("'" ^ e a ^ "', ");
      go st
    | Done -> print "]"
  io_write "[";
  go start

let zip (Stream (f, start)) (Stream (g, start')) =
  let go (sa, sb, x) =
    match x with
    | None ->
        match f sa with
        | Done -> Done
        | Skip sa' -> Skip (sa', sb, None)
        | Yield (x, sa') -> Skip (sa', sb, Some x)
    | Some x ->
        match g sb with
        | Done -> Done
        | Skip sb' -> Skip (sa, sb', Some x)
        | Yield (y, sb') -> Yield ((x, y), sa, sb', None)
  Stream (go, start, start', None)

let ( |> ) x f = f x
let uncurry f (x, y) = f x y

let (>>>) f g = fun x -> g (f x)

let sum_squares xs =
  xs |> map (fun x -> x * x) |> fold_stream (+) 0

let () =
  zip (range (1, 100)) (range (100, 300))
        |> map (uncurry (+))
        |> fold_stream (+) 0
        |> to_string
        |> print

