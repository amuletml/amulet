do
  (nil)({
    op = function(cc) return cc end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(cd) return cd end
  })
end
