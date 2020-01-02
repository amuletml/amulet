open import "prelude.ml"

external val random : int -> int -> int = "math.random"
external val random_seed : int -> unit  = "math.randomseed"

external val current_time : unit -> int  = "function() return os.time() end"

external val prim_int_of_string : string -> int  = "tonumber"

external val is_nil : 'a -> bool = "function(x) return x == nil end"

let int_of_string str =
  let vl = prim_int_of_string str
  if is_nil vl then None else Some vl

let read_line () = Io.(read_line (file_of standard_in))

let () =
  random_seed (current_time ())
  let vl = random 1 10
  let rec loop () =
    put_line "Guess a number between 1 and 10: "
    match read_line () >>= int_of_string with
    | Some guess ->
        if vl == guess then
          put_line "You got it!"
        else if vl > guess then
          put_line "Too low!"
          loop ()
        else if vl < guess then
          put_line "Too high!"
          loop ()
        else loop ()
    | None ->
       put_line "Not a number!"
       loop ()
  loop ()
