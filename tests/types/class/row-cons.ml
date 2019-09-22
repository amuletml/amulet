let x = Amc.extend_row @"y" 2 { x = 1 } 
let y = Amc.extend_row @"x" 2 { x = 1 }
let z = Amc.extend_row @"x" true { x = 1 }
let z = Amc.restrict_row @"x" { x = 1 }
let z = Amc.restrict_row @"y" { x = 1, y = 2 }

(* Before:
  * x : int * { x : int }
  *)
let sound = Amc.restrict_row @"x" (Amc.extend_row @"x" true { x = 1 })
