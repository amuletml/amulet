do
  local bottom = nil
  local print = print
  local bool = true
  local function _amp_amp(a) return function(b) return a and b end end
  local function _bar_bar(a) return function(b) return a or b end end
  local function _not(a) return not a end
  bottom({ ands = _amp_amp, ors = _bar_bar, ["not"] = _not })
  bottom(function(gs)
    if bool then
      return print("L")
    end
    print("R")
    return print("R")
  end)
  bottom(function(hd)
    if not bool then
      return print("R")
    end
    print("L")
    return print("L")
  end)
  if bool then
    print("Hello")
  end
  if not bool then
    print("Hello")
  end
end
