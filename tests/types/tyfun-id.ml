type function id 'a begin
  id 'a = 'a
end

type t 'a = T of 'a

let x : id int = 0

let x : id (int * int) = (0, 0)

let x : id (list int) = [0]

let x : id (t int) = T 0
