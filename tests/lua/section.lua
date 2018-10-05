do
  local function _plus0(bx) return function(by) return bx + by end end
  local main = {
    _1 = _plus0,
    _2 = {
      _1 = function(cb) return 2 * cb end,
      _2 = { _1 = function(d) return d / 2 end, _2 = function(e) return e.foo end }
    }
  }
  local bottom = nil
  bottom(main)
end
