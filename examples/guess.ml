val foreign random "math.random" : int -> int -> int ;
val foreign random_seed "math.randomseed" : int -> unit ;

val foreign print_endline "print" : string -> unit ;
val foreign print "io.write" : string -> unit ;
val foreign read "io.read" : string -> string ;

val foreign current_time "os.time" : unit -> int ;

val foreign prim_int_of_string "tonumber" : string -> int ;
val foreign string_of_int "tostring" : int -> string ;

val foreign transmute "(function(a) return a end)" : 'a -> 'b ;

type option 'a =
  | Just 'a
  | Nothing
  ;

let int_of_string str
  = let vl = prim_int_of_string str
    in if transmute vl == unit then
      Nothing
    else
      Just vl
;

let read_line _ = read "*l"
and main _ =
  begin
    random_seed (current_time unit) ;
    let vl = random 1 10
    and loop _ = begin
      print "Guess a number between 1 and 10: " ;
      let read = int_of_string (read_line unit)
       in match read with
          | Just guess ->
              if vl == guess then
                print_endline "You got it right!"
              else if vl > guess then begin
                   print_endline "Too low!"; loop unit
              end else if vl < guess then begin
                   print_endline "Too high!"; loop unit
              end else loop unit
          | Nothing -> print_endline "Bye"
    end in loop unit
  end
