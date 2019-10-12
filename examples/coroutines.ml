external val (^) : string -> string -> string = ""
type coroutine 'a 'b

external val co_create : ('a -> 'b) -> coroutine 'a 'b = "coroutine.create"
external val co_resume : coroutine 'a 'b -> 'a -> 'b = "coroutine.resume"
external val co_yield : 'b -> 'a = "coroutine.yield"
external val lua_version : string = "_VERSION"

external val print : string -> unit = "print"
external val write : string -> unit = "io.write"
external val string_of_int : int -> string = "tostring"
external val int_of_string : string -> int = "tonumber"
external val read_stdin : string -> string = "io.read"
external val exit : int -> unit = "os.exit"

external val is_nil : 'a -> bool = "function(a) return a == nil end"

type option 'a =
  | Just of 'a
  | Nothing

(* in case you didn't notice, this is a great big hack *)
let parse_num x =
  let number = int_of_string x
  if is_nil number then Nothing else Just number

let () =
  let thrd _ =
    let inner vl =
      match vl with
      | Just x ->
        print ("number was " ^ string_of_int x)
        inner (co_yield ())
      | Nothing ->
        print "got no number"
        exit 0
    inner (co_yield ())
  let coro = co_create thrd
  let loop () =
    write "enter number: "
    let ln = read_stdin "*l"
    let num = parse_num ln
    co_resume coro num
    loop ()
  print ("Amulet, running on " ^ lua_version)
  co_resume coro Nothing
  loop ()
