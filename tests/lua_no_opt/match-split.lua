do
  local ignore = function(x)  end
  local function f1(p)
    return function(k)
      return function(x)
        local function tmp(tmp) return k(3) end
        if x ~= 0 then
          return tmp(nil)
        end
        if p(nil) then
          return k(0)
        end
        return tmp(nil)
      end
    end
  end
  local function f2(p)
    return function(x)
      if x ~= 0 then
        return 3
      end
      if p(nil) then
        return 0
      end
      return 3
    end
  end
  local function f3(p)
    return function(k)
      return function(x)
        local function tmp3(tmp0) return p(nil) end
        local function tmp2(tmp0) return k(2) end
        local function tmp1(tmp0) return k(3) end
        local tmp0 = x._1
        local tmp = x._2
        if tmp0 == 0 then
          return k(0)
        elseif tmp0 == 1 then
          if tmp == 1 then
            if tmp3(nil) then
              return tmp2(nil)
            end
            return tmp1(nil)
          elseif tmp == 2 then
            return k(1)
          else
            return tmp1(nil)
          end
        else
          if tmp ~= 1 then
            return tmp1(nil)
          end
          if tmp3(nil) then
            return tmp2(nil)
          end
          return tmp1(nil)
        end
      end
    end
  end
end
