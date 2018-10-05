do
  local function fib_prime(dn_1, dn_2)
    local cs_2 = 1
    if dn_1 <= cs_2 then
      return dn_2
    else
      return fib_prime(dn_1 - cs_2, dn_2 * dn_1)
    end
  end
  local bottom = nil
  bottom(fib_prime(10, 1))
end
