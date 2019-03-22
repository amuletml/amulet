do
  local print = print
  local function _amp_amp(a) return function(b) return a and b end end
  local function _bar_bar(a) return function(b) return a or b end end
  local function _not(a) return not a end
  (nil)({ ands = _amp_amp, ors = _bar_bar, ["not"] = _not })
  (nil)(function(fw)
    if true then
      return print("L")
    end
    print("R")
    return print("R")
  end)
  (nil)(function(gh)
    if not true then
      return print("R")
    end
    print("L")
    return print("L")
  end)
  if true then
    print("Hello")
  end
  if not true then
    print("Hello")
  end
end
