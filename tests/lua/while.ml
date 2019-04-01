external val print : 'a -> unit = "print"

let foo = ref 1
let units =
  while !foo < 10 do
    foo := !foo + 1
    print !foo
  end
