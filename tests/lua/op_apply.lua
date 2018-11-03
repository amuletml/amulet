do
  local bottom = nil
  bottom({
    op = function(cd) return cd end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(ce) return ce end
  })
end
