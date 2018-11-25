do
  local bottom = nil
  (nil)(function(jg)
    local i, s = jg.i, jg.s
    return {
      int_add_c = 5,
      int_sub_c = -1,
      int_mul_c = 6,
      int_div_c = 3,
      int_exp_c = 8,
      int_eq_c = false,
      int_ne_c = true,
      int_lt_c = true,
      int_gt_c = false,
      int_le_c = false,
      int_ge_c = true,
      int_add_l = i,
      int_add_r = i,
      int_sub_l = i,
      int_mul1_l = i,
      int_mul1_r = i,
      int_mul_l = 0,
      int_mul_r = 0,
      int_div_l = i,
      str_con_c = "foobar",
      str_eq_c = false,
      str_ne_c = true,
      str_con_l = s,
      str_con_r = s,
      bol_eq_c = false,
      bol_ne_c = true,
      uni_eq = true,
      uni_ne = false,
      app = jg.fn(jg.x),
      int_eq_u = true,
      int_ne_u = false,
      str_eq_u = true,
      str_nq_u = false
    }
  end)
end
