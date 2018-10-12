do
  local bottom = nil
  bottom({
    op = function(bw) return bw end,
    app = 2,
    rsec = function(e) return e(2) end,
    lsec = function(bx) return bx end
  })
end
