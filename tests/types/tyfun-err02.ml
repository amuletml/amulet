external val (+) : int -> int -> int = ""

open Amc

class show 'a begin
  val show : 'a -> string
end

instance type_error (String "Can't show functional type" :<>: ShowType ('a -> 'b))
      => show ('a -> 'b)
begin
  let show x = show x
end

let _ = show (fun x -> x + 1)
