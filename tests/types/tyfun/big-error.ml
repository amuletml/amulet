type nat = Z | S of nat

type function length 'xs begin
  length Nil = Z
  length (Cons ('a, 'xs)) = S (length 'xs)
end

type 'a :~: 'b =
  Refl : 'a ~ 'b => 'a :~: 'b


let x : length [1, 2, 3, 4] :~: length [1, 2, 3, 4, 5] = Refl
