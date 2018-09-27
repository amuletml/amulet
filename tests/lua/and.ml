let left && (right : lazy 'a) = if left then force right else false

let main f = f 1 && f 2

external val bottom : 'a = "nil"
let () = bottom main
