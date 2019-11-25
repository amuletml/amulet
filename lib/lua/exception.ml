external val prim_throw : ('exception -> string) -> 'exception -> 'a =
  "function(desc, exc) return error(setmetatable(exc, { __tostring = desc })) end"

external val prim_catch : (unit -> 'a) -> ('exception -> 'a) -> 'a =
  "function(k, h) \
     local ok, err = pcall(k) \
     if not ok then \
       if type(err) ~= 'table' then return error(err) end \
       return h(err) \
     else \
       return err \
     end \
   end"
