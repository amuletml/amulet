class foo 'a | 'a -> 'a begin type bar end

type 'a ~~ 'b = Refl : 'a ~ 'b => 'a ~~ 'b

module Bar_int = begin
  instance foo 'a begin
    type bar = int
  end
  let bar_is_int : forall 'a. bar 'a ~~ int = Refl
end

module Bar_string = begin
  instance foo 'a begin
    type bar = string
  end
  let bar_is_string : forall 'a. bar 'a ~~ string = Refl
end

let sym (Refl : 'a ~~ 'b) : 'b ~~ 'a = Refl
let sub (x : 'p 'a) (Refl : 'a ~~ 'b) : 'p 'b = x

let oh_no : string ~~ int =
  sub (sym (Bar_string.bar_is_string)) Bar_int.bar_is_int

type id 'a = Id of 'a

external val (+) : int -> int -> int = "function(x, y) return x + y end"

let boom =
  let Id my_int = sub (Id "foo") oh_no
  my_int + 123
