external val (^) : string -> string -> string = ""
class show 'a
  val show : 'a -> string

instance show ()
  let show _ = "()"
instance show int
  let show _ = "i just needed another type"

class show 'a => show_but_different 'a
  val show_diff : 'a -> string

instance show 'a => show_but_different 'a
  let show_diff = show

let foo (f : forall 'a. show 'a => 'a -> string) = 
  f () ^ " and " ^ f 123

let f & g = fun x -> f (g x)

let _ = foo (fun x -> (("a" ^) & show) x)
let _ = foo (fun x -> (("a" ^) & show) x)
