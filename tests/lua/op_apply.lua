do
  local bottom = nil
  bottom({
    op = function(bt) return bt end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(bu) return bu end
  })
end
