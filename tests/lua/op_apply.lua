do
  (nil)({
    op = function(f) return f end,
    app = 2,
    rsec = function(r) return r(2) end,
    lsec = function(x) return x end
  })
end
