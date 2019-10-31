let x :: xs = Cons (x, xs)

let rec zip2 xs ys =
  match xs, ys with
  | Cons (x, xs), Cons (y, ys) -> (x, y) :: zip2 xs ys
  | _, _ -> []

class zip 'a 'b 'c | 'c -> 'a, 'c -> 'b begin
  val zip : list 'a -> list 'b -> 'c
end

instance zip 'a 'b (list ('a * 'b)) begin
  let zip = zip2
end

instance zip ('a * 'b) 'c 'e => zip 'a 'b (list 'c -> 'e) begin
  let zip xs ys zs = zip (zip2 xs ys) zs
end

let z3 (xs : list 'a) (ys : list 'b) (zs : list 'c) : list ('a * 'b * 'c) =
  zip xs (zip ys zs)
