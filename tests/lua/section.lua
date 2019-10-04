do
  local function _plus0(cb) return function(cc) return cb + cc end end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function(cf) return 2 * cf end,
      _2 = { _1 = function(d) return d / 2 end, _2 = function(e) return e.foo end }
    }
  }
  (nil)(main)
end
