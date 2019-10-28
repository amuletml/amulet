class foo 'a begin
  type bar : type
  val into : 'a -> bar 'a
end

instance foo string begin
  type bar = int
  let into _ = 123
end

instance foo unit begin
  type bar = string
  let into _ = ""
end

let _ : bar unit = ""
let _ : bar string = ()
