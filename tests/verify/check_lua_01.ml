external val x : int = "x x"

external val x : int = "return 0"

external val x : int -> int = "function(x)\
  for i = 0, 10 do\
    x = i + \
  end\
  return i\
end"
