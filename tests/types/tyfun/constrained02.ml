class foo 'a begin
  type bar
end

type bar_box 'a =
  | MkBox : bar 'a -> bar_box 'a
