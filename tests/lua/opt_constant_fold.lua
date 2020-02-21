do
  local ignore = function(x)  end
  ignore(function(tmp)
    local i, s = tmp.i, tmp.s
    return {
      int_add_c = 5,
      int_sub_c = -1,
      int_mul_c = 6,
      int_div_c = 3.0,
      int_exp_c = 8,
      int_eq_c = false,
      int_lt_c = true,
      int_le_c = true,
      int_add_l = i,
      int_add_r = i,
      int_sub_l = i,
      int_mul1_l = i,
      int_mul1_r = i,
      int_mul_l = 0,
      int_mul_r = 0,
      int_div_l = i,
      float_add_c = 5.0,
      float_sub_c = -1.0,
      float_mul_c = 6.0,
      float_div_c = 3.0,
      float_exp_c = 8.0,
      float_eq_c = false,
      float_lt_c = true,
      float_le_c = true,
      str_con_c = "foobar",
      str_eq_c = false,
      str_le_c = false,
      str_lt_c = false,
      str_con_l = s,
      str_con_r = s,
      int_eq_u = true,
      str_eq_u = true
    }
  end)
end
