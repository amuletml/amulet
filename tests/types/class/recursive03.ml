external val (^) : string -> string -> string = ""

external val prim_show : 'a -> string = "tostring"
external val print : string -> unit = "print"

class show 'a begin
  val show : 'a -> string
end

instance show 'a => show (list 'a) begin
  let show = function
    | Nil -> "[]"
    | Cons (x, xs) -> show x ^ " :: " ^ show xs
end

instance show int begin
  let show = prim_show
end

let foo (x : 'a) : unit =
  print (show x)
  bar [x]
and bar x = foo x


let () = foo 1
