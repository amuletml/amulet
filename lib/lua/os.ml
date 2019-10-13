type time

external val clock : unit -> float = "os.clock"

external val strftime : string -> time -> string =
  "function(spec, time) \
     if (spec == '*t') or (spec == '!*t') then \
       return os.date((spec == '!*t') and '!' or '%c', time) \
     else \
       return os.date(spec, time) \
     end\
   end"

external val exit_success : unit -> 'a =
  "function() return os.exit(0) end"

external val exit_failure : unit -> 'a =
  "function() return os.exit(1) end"

external val getenv : string -> (string -> 'a) -> 'a -> 'a =
  "function(x, some, none) \
    return ((type(os.getenv(x)) == 'string') and some(os.getenv(x))) or none\
   end"

external val time_now : unit -> time = "function() return os.time() end"
external val diff_time : time -> time -> time = "os.difftime"
