external val (^) : string -> string -> string = ""

class show 'a begin
  val show : 'a -> string
end

instance show 't => show (list 't) begin
  let show xs =
    let rec f = function
      | [x]            -> show x
      | (Cons (x, xs)) -> show x ^ "," ^ f xs
    in "[" ^ f xs ^ "]"
end
