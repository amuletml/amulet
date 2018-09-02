do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local function _plus0(bu) return function(bv) return bu + bv end end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function(by) return 2 * by end,
      _2 = { _1 = function(d) return d / 2 end, _2 = function(e) return e.foo end }
    }
  }
  local bottom = nil
  bottom(main)
end
