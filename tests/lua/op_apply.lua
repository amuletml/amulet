do
  (nil)({
    op = function(cj) return cj end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(ck) return ck end
  })
end
