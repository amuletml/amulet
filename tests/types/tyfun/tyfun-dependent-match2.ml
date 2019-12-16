type function empty ('x : type) : 'x begin
  empty int = 0
  empty string = ""
  empty (list 'a) = []
end
