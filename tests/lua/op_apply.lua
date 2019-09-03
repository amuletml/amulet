do
  (nil)({
    op = function(ce) return ce end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(cf) return cf end
  })
end
