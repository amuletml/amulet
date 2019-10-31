let pure x = [x]
let fs <*> xs = [ f x | with f <- fs, with x <- xs ]

let cartesian xs ys = (| (,) xs ys |)

let x :: xs = Cons (x, xs)

let traverse cont =
  let rec loop = function
    | Cons (x, xs) -> (| cont x :: loop xs |)
    | [] -> (| [] |)
  loop
