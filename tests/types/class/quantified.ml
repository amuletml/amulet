external val (^) : string -> string -> string = ""

class show 'a
  val show : 'a -> string

type box 'f 'a = Box of 'f 'a

instance (forall 'a. show 'a => show ('f 'a)) * show 'a => show (box 'f 'a)
  let show (Box f) = "(Box " ^ show f ^ ")"

type id 'a = Id of 'a

instance show 'a => show (id 'a)
  let show (Id x) = "(Id " ^ show x ^ ")"

instance show unit
  let show () = "()"

let "(Box (Id ()))" = show (Box (Id ()))
