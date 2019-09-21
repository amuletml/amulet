external val io_write : string -> unit = "io.write" ;;
external val print : string -> unit = "print" ;;
external val to_string : 'a -> string = "tostring" ;;
(* this type is a lie *)

type step 's 'a =
  | Skip of 's
  | Yield of 'a * 's
  | Done ;;

type stream 'a =
  | Stream : forall 's. ('s -> step 's 'a) * 's -> stream 'a ;;

let range (start, limit) =
  let go n =
    if n > limit then
      Done
    else
      Yield (n, n + 1)
  Stream (go, start)

let dump_stream e (Stream (f, start)) =
  let go st =
    match f st with
    | Skip st -> go st
    | Yield (a, st) ->
      io_write ("'" ^ e a ^ "', ")
      go st
    | Done -> print "]"
  io_write "["
  go start

let (|>) x f = f x

let main = range (1, 5) |> dump_stream to_string
(* With `match` commuting conversion, this will do a single pass over the input
 * stream (the range, in this case), and the resulting code will have no mention
 * of Stream, Skip, Yield or Done (even though at this time the compiler will
 * generate code for them)
 *)

external val ignore : 'a -> () = "nil"
let () = ignore main
