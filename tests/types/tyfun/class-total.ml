class loop begin
  type loop_t
end

instance loop begin
  type loop_t = list loop_t
end

type void = |

type function equal 'a 'b begin
  equal 'a 'a = ()
  equal 'a 'b = void
end

let x : equal loop_t (list loop_t) = ()
