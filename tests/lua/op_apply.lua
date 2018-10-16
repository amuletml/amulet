do
  local bottom = nil
  bottom({
    op = function(by) return by end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(bz) return bz end
  })
end
