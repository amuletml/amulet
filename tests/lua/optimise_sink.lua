do
  local Nil = { __tag = "Nil" }
  local function main(x)
    if x.__tag == "Nil" then
      return function(cp) return { _1 = 1, _2 = x } end
    end
    return function(x0) return { _1 = x0, _2 = Nil } end
  end
  (nil)(main)
end
