let main { x } = x + main { x }
let _ = main { x = 1 }
