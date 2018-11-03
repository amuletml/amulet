class sized 'a begin
  val size : int

  val dummy : 'a
  let dummy = dummy
end

instance sized unit begin
  let size = 0
end

let 0 = size @_ @unit
