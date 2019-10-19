do
  local function _plus0(tmp) return function(tmp0) return tmp + tmp0 end end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function(tmp) return 2 * tmp end,
      _2 = { _1 = function(r) return r / 2 end, _2 = function(tbl) return tbl.foo end }
    }
  }
  local cu = main._2
  local cs = cu._2;
  (nil)({ _1 = main._1, _2 = { _2 = { _1 = cs._1, _2 = cs._2 }, _1 = cu._1 } })
end
