do
  (nil)({
    op = function(tmp) return tmp end,
    app = 2,
    rsec = function(r) return r(2) end,
    lsec = function(tmp) return tmp end
  })
end
