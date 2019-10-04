type function foo 'a begin end
type function bar 'a begin
  bar (foo int) = string
end
