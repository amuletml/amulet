class show 'a begin
  val show : 'a -> string
end

instance show () begin
  let show _ = "()"
end

instance show string begin
  let show x = x
end

type show_box = Box : show 'a => 'a -> show_box

let xs = [ Box (), Box "" ]
