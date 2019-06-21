class sized 'a begin
  val size : 'proxy 'a -> int
end

instance sized unit begin
  let size _ = 0
end

let 0 = size @unit []
