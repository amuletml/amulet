open import "amulet/base.ml"

let reset = "\x1b[0m"

let bold s = "\x1b[1m" ^ s ^ reset
let underline s = "\x1b[4m" ^ s ^ reset
let invert s = "\x1b[7m" ^ s ^ reset

let black = "\x1b[39m"
let red s = "\x1b[31m" ^ s ^ black
let green s = "\x1b[32m" ^ s ^ black
let yellow s = "\x1b[33m" ^ s ^ black
let blue s = "\x1b[34m" ^ s ^ black
let magenta s = "\x1b[35m" ^ s ^ black
let cyan s = "\x1b[36m" ^ s ^ black
let gray s = "\x1b[90m" ^ s ^ black
let white s = "\x1b[90m" ^ s ^ black

let light_red s = "\x1b[91m" ^ s ^ black
let light_green s = "\x1b[92m" ^ s ^ black
let light_yellow s = "\x1b[93m" ^ s ^ black
let light_blue s = "\x1b[94m" ^ s ^ black
let light_magenta s = "\x1b[95m" ^ s ^ black
let light_cyan s = "\x1b[96m" ^ s ^ black
let light_gray s = "\x1b[37m" ^ s ^ black

let bg_default = "\x1b[49m"
let bg_red s = "\x1b[41m" ^ s ^ bg_default
let bg_green s = "\x1b[42m" ^ s ^ bg_default
let bg_yellow s = "\x1b[43m" ^ s ^ bg_default
let bg_blue s = "\x1b[44m" ^ s ^ bg_default
let bg_magenta s = "\x1b[45m" ^ s ^ bg_default
let bg_cyan s = "\x1b[46m" ^ s ^ bg_default
let bg_gray s = "\x1b[100m" ^ s ^ bg_default
let bg_white s = "\x1b[107m" ^ s ^ bg_default


let bg_light_red s = "\x1b[101m" ^ s ^ bg_default
let bg_light_green s = "\x1b[102m" ^ s ^ bg_default
let bg_light_yellow s = "\x1b[103m" ^ s ^ bg_default
let bg_light_blue s = "\x1b[104m" ^ s ^ bg_default
let bg_light_magenta s = "\x1b[105m" ^ s ^ bg_default
let bg_light_cyan s = "\x1b[106m" ^ s ^ bg_default
let bg_light_gray s = "\x1b[47m" ^ s ^ bg_default
