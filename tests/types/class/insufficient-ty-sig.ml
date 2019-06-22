class eq 'a begin
  val (=.=) : 'a -> 'a -> bool
end

let foo : forall 'a. 'a -> 'a -> bool =
  (=.=)
