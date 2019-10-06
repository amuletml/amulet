class has_sing 'k begin
  type sing_t : 'k -> type
end

type sint ('i : int) =
  | SInt : Amc.known_int 'i => sint 'i

instance has_sing int begin
  type sing_t = sint
end

let sing : sing_t int _ = SInt @123
