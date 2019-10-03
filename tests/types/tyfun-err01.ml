open Amc

type function foo 'a begin
  foo int = string
  foo string = int
  foo 'a = type_error (String "Unexpected argument to foo:" :<>: ShowType 'a)
end

let foo : foo bool = ()
