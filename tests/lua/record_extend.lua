do
  local __builtin_unit = { __tag = "__builtin_unit" }
  local bottom = nil
  local ax = {}
  for k, v in pairs(bottom) do
    ax[k] = v
  end
  ax.x = 1
  bottom(ax)
end
