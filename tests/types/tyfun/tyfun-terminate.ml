type t = A | B | C | D of constraint | E | F of type

type function really_recursive ('a : t) begin
  really_recursive A      = (really_recursive A * really_recursive A)
  really_recursive B      = really_recursive B -> ()
  really_recursive C      = forall 'a. really_recursive C -> 'a
  really_recursive (D 'c) = 'c => really_recursive (D 'c)
  really_recursive E      = { x : really_recursive E }
  really_recursive (F 'a) = { 'a | x : really_recursive (F 'a) }
end
