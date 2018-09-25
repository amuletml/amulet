let f x = g x + 1
and g x = f x + 1

external val bottom : 'a = "nil"
let () = bottom f
