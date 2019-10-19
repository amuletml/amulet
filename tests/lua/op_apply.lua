do
  local function _at_at(tmp) return function(tmp0) return tmp(tmp0) end end
  local function tmp(x) return x end
  (nil)({
    rsec = function(r) return r(2) end,
    lsec = function(tmp0) return tmp(tmp0) end,
    op = _at_at,
    app = 2
  })
end
