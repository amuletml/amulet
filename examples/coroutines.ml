type coroutine 'a 'b;

external val co_create : ('a -> 'b) -> coroutine 'a 'b = "coroutine.create" ;
external val co_resume : coroutine 'a 'b -> 'a -> 'b = "coroutine.resume" ;
external val co_yield : 'b -> 'a = "coroutine.yield" ;
external val lua_version : string = "_VERSION" ;

external val print : string -> unit = "print" ;
external val write : string -> unit = "io.write" ;
external val string_of_int : int -> string = "tostring" ;
external val int_of_string : string -> int = "tonumber" ;
external val read_stdin : string -> string = "io.read" ;
external val exit : int -> unit = "os.exit" ;

(* i'm impressed, but not surprised, that this works *)
external val unsafe_coerce : 'a -> 'b = "(function(a) return a end)" ;

type option 'a =
  | Just of 'a
  | Nothing ;

(* in case you didn't notice, this is a great big hack *)
let parse_num x
  = let number = int_of_string x
    in if (unsafe_coerce number == ()) then 
       Nothing
     else
       Just number ;

let main _ =
  let thrd _ = begin
    let inner vl =
      match vl with
      | Just x -> begin
        print ("number was " ^ string_of_int x);
        inner (co_yield ())
      end
      | Nothing -> begin
        print "got no number";
        exit 0
      end
     in inner (co_yield ())
  end
  and coro = co_create thrd
  and loop (_ : unit) = begin
    write "enter number: ";
    let ln = read_stdin "*l"
    and num = parse_num ln
      in begin
        co_resume coro num;
        loop ()
      end
  end in begin
    print ("Amulet, running on " ^ lua_version);
    co_resume coro Nothing;
    loop ()
  end
