let main = function
  | { a = { a } as b, c } -> a + b.b + c

external val bottom : 'a = "nil"
let () = bottom main
