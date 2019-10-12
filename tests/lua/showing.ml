external val ( ^ ) : string -> string -> string = "function(x, y) return x .. y end"
external val print : 'a -> unit = "print"
external val int_to_str : int -> string = "int_to_str"

class show 'a begin
 val show : 'a -> string
end

instance show string begin
  let show x = x
end

instance show () begin
  let show () = "()"
end

instance show int begin
  let show = int_to_str
end

let show_cat x y z = show x ^ show y ^ show z

let foo f = f () "foo" 1

let () = print (foo show_cat)
