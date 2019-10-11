let pure x = [x]
let fs <*> xs = [ f x | with f <- fs, with x <- xs ]
