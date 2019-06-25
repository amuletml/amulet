do
  local function _plus0(bz) return function(ca) return bz + ca end end
  local main = {
    _1 = _plus0,
    _2 = {
      _2 = { _1 = function(d) return d / 2 end, _2 = function(e) return e.foo end },
      _1 = function(cd) return 2 * cd end
    }
  }
  (nil)(main)
end
