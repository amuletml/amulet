do
  local function fib_prime(ds_1, ds_2)
    local cx_2 = 1
    if ds_1 <= cx_2 then
      return ds_2
    else
      return fib_prime(ds_1 - cx_2, ds_2 * ds_1)
    end
  end
  local bottom = nil
  bottom(fib_prime(10, 1))
end
