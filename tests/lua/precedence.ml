external val bottom : 'a = "nil"

let main { a, b, c } = (a + b) * c
let () = bottom main
