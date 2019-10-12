external val (==) : int -> int -> bool = ""

class eq 'a begin
  val (===) : 'a -> 'a -> bool
end

class eq (elem 'a) => collection 'a begin
  type elem
  val empty : 'a
  val insert : elem 'a -> 'a -> 'a
  val memq : elem 'a -> 'a -> bool
end

instance eq int begin
  let (===) = (==)
end

instance eq 'a => collection (list 'a) begin
  type elem = 'a
  let empty = []
  let insert x xs = Cons (x, xs)
  let memq x = function
    | [] -> false
    | Cons (y, xs) ->
        if x === y then
          true
        else
          memq x xs
end

let foo = insert 1 (insert 2 [])
