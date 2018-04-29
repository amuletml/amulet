external val io_write : string -> unit = "io.write" ;;
external val print : string -> unit = "print" ;;
external val to_string : 'a -> string = "tostring" ;;
external val rem : int -> int -> int = "function(x, y) return x % y end" ;;

type step 's 'a =
  | Skip of 's
  | Yield of 'a * 's
  | Done ;;

type stream 'a =
  | Stream : forall 's. ('s -> step 's 'a) * 's -> stream 'a ;;

let take len (Stream (f, start)) =
  let go (i, st) =
    if i <= len then
      match f st with
      | Done -> Done
      | Skip s -> Skip (i + 1, s)
      | Yield (a, s) -> Yield (a, i + 1, s)
    else
      Done
  in Stream (go, 1, start) ;;

let filter p (Stream (f, start)) =
  let go st =
    match f st with
    | Done -> Done
    | Skip s -> Skip s
    | Yield (a, s) -> if p a then
      Yield (a, s)
    else
      Skip s
  in Stream (go, start) ;;

let range (start, limit) =
  let go n =
    if n > limit then
      Done
    else
      Yield (n, n + 1)
  in Stream (go, start) ;;

let fold_stream f z (Stream (stream, start)) =
  let go ac st =
    match stream st with
    | Yield (a, st) -> go (f a ac) st
    | Skip st -> go ac st
    | Done -> ac
  in go z start ;;

let dump_stream e (Stream (f, start)) =
  let go st =
    match f st with
    | Skip st -> go st
    | Yield (a, st) -> begin
      io_write ("'" ^ e a ^ "', ");
      go st
    end
    | Done -> print "]"
  in begin
    io_write "[";
    go start
  end ;;

let (|>) x f = f x ;;

let main = range (1, 10000000)
    |> take 100
    |> filter (fun x -> rem x 2 == 0)
    |> fold_stream (+) 0
    |> to_string
    |> print
