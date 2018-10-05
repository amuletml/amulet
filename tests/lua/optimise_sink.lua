do
  local Nil = { __tag = "Nil" }
  local function Cons(x) return { __tag = "Cons", x } end
  local function main(x)
    if x.__tag == "Nil" then
      return function(dn) return { _1 = 1, _2 = x } end
    elseif x.__tag == "Cons" then
      return function(x0) return { _1 = x0, _2 = Nil } end
    end
  end
  local bottom = nil
  bottom(main)
end
