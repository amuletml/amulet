external val random : int -> int -> int = "math.random"
external val random_seed : int -> unit  = "math.randomseed"

external val print_endline : string -> unit = "print"
external val print : string -> unit  = "io.write"
external val read : string -> string  = "io.read"

external val current_time : unit -> int  = "function() return os.time() end"

external val prim_int_of_string : string -> int  = "tonumber"
external val string_of_int : int -> string  = "tostring"

external val is_nil : 'a -> bool = "function(x) return x == nil end"

type option 'a =
  | Just of 'a
  | Nothing

let int_of_string str =
  let vl = prim_int_of_string str
  if is_nil vl then Nothing else Just vl

let read_line _ = read "*l"
let () =
  random_seed (current_time ())
  let vl = random 1 10
  let loop () =
    print "Guess a number between 1 and 10: "
    match int_of_string (read_line ()) with
    | Just guess ->
        if vl == guess then
          print_endline "You got it!"
        else if vl > guess then
          print_endline "Too low!"
          loop ()
        else if vl < guess then
          print_endline "Too high!"
          loop ()
        else loop ()
    | Nothing ->
       print_endline "Not a number!"
       loop ()
  loop ()
