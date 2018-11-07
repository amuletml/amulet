do
  local bottom = nil
  bottom({
    op = function(ci) return ci end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(cj) return cj end
  })
end
