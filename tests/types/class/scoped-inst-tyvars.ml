class show 'a begin
  val show : 'a -> string
end

instance show 'a => show (list 'a) begin
  let show xs =
    let show_elem : 'a -> string = show
    let go acc = function
      | [] -> acc ^ "]"
      | Cons (x, xs) -> go (acc ^ ", " ^ show_elem x) xs
   go "[" xs
end

