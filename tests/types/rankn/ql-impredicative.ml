let x :: xs = Cons (x, xs)

let id_list : list (forall 'a. 'a -> 'a) = []
let ids = (fun x -> x) :: id_list
