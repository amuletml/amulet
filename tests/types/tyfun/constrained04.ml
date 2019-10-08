class c2 't begin
  type f2
end

instance c2 int begin
  type f2 = bool
end

instance c2 (list 'a) begin
  type f2 = f2 'a
end

(* type foo = Foo of f2 'a *)
