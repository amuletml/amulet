do
  local function _plus0(cd) return function(ce) return cd + ce end end
  local main = {
    _1 = _plus0,
    _2 = {
      _2 = { _1 = function(d) return d / 2 end, _2 = function(e) return e.foo end },
      _1 = function(ch) return 2 * ch end
    }
  }
  (nil)(main)
end
