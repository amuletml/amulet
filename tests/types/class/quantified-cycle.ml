type t1 'a = T1
type t2 'a = T2

class c 'a begin
  val meth : 'a -> ()
  let meth _ = ()
end

instance (forall 'a. c (t1 'a)) => c (t2 'a) begin end
instance (forall 'a. c (t2 'a)) => c (t1 'a) begin end

let () = meth T1
