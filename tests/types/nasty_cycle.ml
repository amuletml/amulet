type nat = Z | S of nat
type snat 'n =
  | SZ : snat Z
  | SS : snat 's -> snat (S 's)

type even 'n =
  | Even0 : even Z
  | Even2 : even 's -> even (S (S 's))

type either 'l 'r = Left of 'l | Right of 'r

let is_even (x : snat 'n) : either (even 'n) (even (S 'n)) =
  match x with
  | SZ -> Left Even0
  | SS SZ -> Right (Even2 Even0)
  | SS (SS x) ->
      match is_even x with
      | Left p -> Right (Even2 p)
      | Right p -> Left p
