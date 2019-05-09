class eq 'a begin
  val (==) : 'a -> 'a -> bool
end

let foo =
  fun x -> if x == "foo" then "foo" else "bar"
