do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  bottom({
    op = function(bt) return bt end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(bu) return bu end
  })
end
