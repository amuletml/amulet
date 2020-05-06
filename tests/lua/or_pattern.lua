do
  local function Some(a) return { __tag = "Some", a } end
  local None = { __tag = "None" }
  local ignore = ignore
  ignore(function(tmp)
    local num, opt = tmp.num, tmp.opt
    if num == 1 then
      local tmp0 = opt._1
      local tmp1 = opt._2
      ignore(0)
      if tmp0.__tag == "None" then return ignore(None) end
      local x = tmp0[1]
      if tmp1.__tag == "None" then return ignore(None) end
      return ignore(Some({ _1 = x, _2 = tmp1[1] }))
    elseif num == 2 then
      local tmp0 = opt._1
      local tmp1 = opt._2
      ignore(0)
      if tmp0.__tag == "None" then return ignore(None) end
      local x = tmp0[1]
      if tmp1.__tag == "None" then return ignore(None) end
      return ignore(Some({ _1 = x, _2 = tmp1[1] }))
    elseif num == 3 then
      local tmp0 = opt._1
      local tmp1 = opt._2
      ignore(0)
      if tmp0.__tag == "None" then return ignore(None) end
      local x = tmp0[1]
      if tmp1.__tag == "None" then return ignore(None) end
      return ignore(Some({ _1 = x, _2 = tmp1[1] }))
    else
      local tmp0 = opt._1
      local tmp1 = opt._2
      ignore(num)
      if tmp0.__tag == "None" then return ignore(None) end
      local x = tmp0[1]
      if tmp1.__tag == "None" then return ignore(None) end
      return ignore(Some({ _1 = x, _2 = tmp1[1] }))
    end
  end)
end
