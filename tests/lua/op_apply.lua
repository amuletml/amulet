do
  (nil)({
    op = function(cf) return cf end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(cg) return cg end
  })
end
