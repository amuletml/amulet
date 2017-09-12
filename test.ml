type coroutine 'a 'b =;

val foreign co_create "coroutine.create" : ('a -> 'b) -> coroutine 'a 'b;
val foreign co_resume "coroutine.resume" : coroutine 'a 'b -> 'a -> 'b;
val foreign co_yield "coroutine.yield" : 'b -> 'a;
val foreign lua_version "_VERSION" : string;

val foreign print "print" : string -> unit;
val foreign write "io.write" : string -> unit;
val foreign string_of_int "tostring" : int -> string;
val foreign int_of_string "tonumber" : string -> int;
val foreign read_stdin "io.read" : string -> string;
val foreign exit "os.exit" : int -> unit;

(* i'm impressed, but not surprised, that this works *)
val foreign unsafe_coerce "(function(a) return a end)" : 'a -> 'b;

type option 'a =
  | Just 'a
  | Nothing ;

(* in case you didn't notice, this is a great big hack *)
let parse_num x
  = let number = int_of_string x
    in if (unsafe_coerce number == unit) then 
       Nothing
     else
       Just number ;

let main _ =
  let thrd _ = begin
    let inner vl =
      match vl with
      | Just x -> begin
        print ("number was " ^ string_of_int x);
        inner (co_yield unit)
      end
      | Nothing -> begin
        print "got no number";
        exit 0
      end
     in inner (co_yield unit)
  end
  and coro = co_create thrd
  and loop (_ : unit) = begin
    write "enter number: ";
    let ln = read_stdin "*l"
    and num = parse_num ln
      in begin
        co_resume coro num;
        loop unit
      end
  end in begin
    print ("Amulet, running on " ^ lua_version);
    co_resume coro Nothing;
    loop unit
  end
