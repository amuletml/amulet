-- There was a problem where the parser did not know if curried
-- statements were multiple calls or subsequent ones.

a()()
a(1)(2)

local _ = a()()
local _ = a(1)(1)
