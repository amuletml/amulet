do
  local function _at_at(tmp) return function(tmp0) return tmp(tmp0) end end
  local function tmp(x) return x end
  (nil)({
    op = _at_at,
    rsec = function(r) return r(2) end,
    lsec = function(tmp0) return tmp(tmp0) end,
    app = 2
  })
end
