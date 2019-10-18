type function get_con 'a begin
  get_con ('f 'a) = 'f
end

type 'a ~~ 'b = Refl : 'a ~~ 'b

let _ : (get_con (list int) ~~ list) = Refl
