let x = Amc.extend_row @"y" { x = 1 } 2
let y = Amc.extend_row @"x" { x = 1 } 2
let z = Amc.extend_row @"x" { x = 1 } true
let z = Amc.restrict_row @"x" { x = 1 }
let z = Amc.restrict_row @"y" { x = 1, y = 2 }
