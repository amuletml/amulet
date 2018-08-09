let main x = { x with x = 1 }

external val bottom : 'a = "nil"
let () = bottom (main bottom)
