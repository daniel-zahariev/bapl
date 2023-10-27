-- print((I"A" * lpeg.S"aA" * I"B" * lpeg.S"bB" * I"C" * lpeg.R"cC" * I"+" ):match"abc")

local lpeg = require"lpeg"
lpeg.locale(lpeg)

function I (msg)
  return lpeg.P(function (_, p) print (msg, p); return true end)
end

local keywords = {"if", "elseif", "else", "end"}
local alnum = lpeg.alnum^1
local keyword = lpeg.P(false)
for _, kw in ipairs(keywords) do
  keyword = keyword + I(kw)*kw*I("end")
end
keyword = keyword * I("A")*-alnum^-1*I("B")

-- make these assertions to pass
assert(lpeg.match(keyword, "else") == 5)
assert(lpeg.match(keyword, "elseif") == 7)
assert(lpeg.match(keyword, "else1") == nil)
assert(lpeg.match(keyword, "else;") == nil)


local open = lpeg.P('"')
-- local close = open
local close = -lpeg.B("\\", 1) * open
local p = open * lpeg.C((1 - close)^0) * close
-- make these assertions to pass
assert([[al"b]] == p:match([["a\"b"]]))
-- assert([[a\\]] == p:match([["a\\"]]))
-- assert([[xyz\\]] == p:match([["xyz\\"]]))