external val (^) : string -> string -> string = ""
external val (+) : int -> int -> int = ""
type foo 'a = Foo of 'a
class show 'a begin
  val show : 'a -> string
end

instance show ()
  let show () = "()"

instance show 'a => show (foo 'a)
  let show (Foo x) = "Foo " ^ show x

let "Foo ()" = show (Foo ())
