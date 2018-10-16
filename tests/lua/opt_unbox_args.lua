do
  local function fib_prime(dv_1, dv_2)
    local da_2 = 1
    if dv_1 <= da_2 then
      return dv_2
    else
      return fib_prime(dv_1 - da_2, dv_2 * dv_1)
    end
  end
  local bottom = nil
  bottom(fib_prime(10, 1))
end
