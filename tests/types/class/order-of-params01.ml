class show 'a begin
  val show : 'a -> string
end

instance show string begin
  let show x = x
end

instance show () begin
  let show () = "()"
end

let show_cat x y = show x ^ show y
let foo f = f () "foo"
let "" = foo show_cat
