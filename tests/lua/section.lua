do
  local function _plus0(cc) return function(cd) return cc + cd end end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function(cg) return 2 * cg end,
      _2 = { _1 = function(d) return d / 2 end, _2 = function(e) return e.foo end }
    }
  }
  local bottom = nil
  bottom(main)
end
