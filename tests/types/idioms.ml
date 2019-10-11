let pure x = [x]
let fs <*> xs = [ f x | with f <- fs, with x <- xs ]

let cartesian xs ys = (| (,) xs ys |)
