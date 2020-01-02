open import "prelude.ml"

type coroutine 'a 'b

external val co_create : ('a -> 'b) -> coroutine 'a 'b = "coroutine.create"
external val co_resume : coroutine 'a 'b -> 'a -> 'b = "coroutine.resume"
external val co_yield : 'b -> 'a = "coroutine.yield"
external val lua_version : string = "_VERSION"

external val int_of_string : string -> int = "tonumber"
external val exit : int -> unit = "os.exit"

external val is_nil : 'a -> bool = "function(a) return a == nil end"

(* in case you didn't notice, this is a great big hack *)
let parse_num x =
  let number = int_of_string x
  if is_nil number then None else Some number

let read_line () = Io.(read_line (file_of standard_in))

let () =
  let thrd _ =
    let rec inner : option int -> () = function
      | Some x ->
        put_line ("number was " ^ show x)
        inner (co_yield ())
      | None ->
        put_line "got no number"
        exit 0
    inner (co_yield ())
  let coro = co_create thrd
  let rec loop () =
    put_line "enter number: "
    match read_line () with
    | None -> ()
    | Some ln ->
      let num = parse_num ln
      co_resume coro num
      loop ()
  put_line ("Amulet, running on " ^ lua_version)
  co_resume coro None
  loop ()
