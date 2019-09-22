do
  local function _plus0(ca) return function(cb) return ca + cb end end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function(ce) return 2 * ce end,
      _2 = { _1 = function(d) return d / 2 end, _2 = function(e) return e.foo end }
    }
  }
  (nil)(main)
end
