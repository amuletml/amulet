do
  local _print = print
  local function _amp_amp(a) return function(b) return a and b end end
  local function _bar_bar(a) return function(b) return a or b end end
  local function _not(a) return not a end
  (nil)({ ands = _amp_amp, ors = _bar_bar, ["not"] = _not });
  (nil)(function(tmp)
    if true then return _print("L") end
    _print("R")
    return _print("R")
  end);
  (nil)(function(tmp)
    if not true then return _print("R") end
    _print("L")
    return _print("L")
  end)
  if true then _print("Hello") end
  if not true then _print("Hello") end
  (nil)(function(x)
    if x == 0 then return nil end
    _print("Not 0")
    return _print("Extra text")
  end)
end
