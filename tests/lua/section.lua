do
  local function _plus0(ce) return function(cf) return ce + cf end end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function(ci) return 2 * ci end,
      _2 = { _1 = function(d) return d / 2 end, _2 = function(e) return e.foo end }
    }
  }
  local bottom = nil
  (nil)(main)
end
