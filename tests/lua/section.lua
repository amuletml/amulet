do
  local function _plus0(tmp) return function(tmp0) return tmp + tmp0 end end
  local main = {
    _1 = _plus0,
    _2 = {
      _2 = { _1 = function(r) return r / 2 end, _2 = function(tbl) return tbl.foo end },
      _1 = function(tmp) return 2 * tmp end
    }
  };
  (nil)(main)
end
