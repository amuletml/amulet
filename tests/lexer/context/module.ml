(* Implicit begin *)
module X =
  let _ = 1

(* Explicit begin *)
module X = begin
  let _ = 1
end

(* Explicit begin with weird alignment *)
module X =
begin
  let _ = 1
end

module X =
  begin
    let _ = 1
  end

module X = Y

(* Access modifiers *)
private module Y =
  let a = 0
