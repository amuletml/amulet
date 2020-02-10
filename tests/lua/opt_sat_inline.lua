do
  local E = { __tag = "E" }
  local function foldr_sat(zero, x)
    if x.__tag ~= "T" then return zero end
    local tmp = x[1]
    local tmp0 = tmp._2._2
    return foldr_sat(tmp._1 + foldr_sat(zero, tmp0._2), tmp0._1)
  end
  foldr_sat(0, E)
end
