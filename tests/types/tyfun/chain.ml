type function bar 'a begin
  bar int = int
end

type function foo 'a begin
  foo 'a = bar 'a
end

let x : foo int = 123
