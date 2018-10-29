let x = 1
and y = 2
and z = 3

(* Expressions *)
let basic_record = { x = 1, y = 2, z : int = 3 }

let short_record = { x, y, z : int }

let trail_record = { x = 1, y, }

let basic_extension = { basic_record with y = 3 }

let short_extension = { short_record with y }

let trail_extension = { short_record with y, }

(* Patterns *)
let basic_pattern = function
  | { x = 1, y = y } -> y

let short_pattern = function
  | { x, y } -> y

let trail_pattern = function
  | { x = 1, y, } -> y

(* Types *)
let record_type : {  x: int, y : int, z : int } = a

let record_type_trail : { x: int, y : int, } = a

let record_extension_type : { 'a | x: int, y : int } = a

let record_extension_type_empty : { 'a | } = a

let record_extension_type_trail : { 'a | x: int, y : int, } = a
