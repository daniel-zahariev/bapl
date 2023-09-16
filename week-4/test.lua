_G.package.path = _G.package.path .. ";../utils/?.lua"
local test = require "u-test" -- https://github.com/IUdalov/u-test
local lang = require "lang"

-- unary plus is curretly treated as a noop
test.unary_plus = function()
  test.equal(lang.execute("return +1"), 1)
  test.equal(lang.execute("x=12 + +1; return x"), 13)
  test.equal(lang.execute("x=12 + ++++1; return x"), 13)
  -- shouldn't modify negative values
  test.equal(lang.execute("x=-5; return +x"), -5)
end

-- unary minus should apply to numbers, variables, and expressions
test.unary_minus = function()
  test.equal(lang.execute("return -1"), -1)
  test.equal(lang.execute("x = -5; return x"), -5)
  test.equal(lang.execute("x = 2; return -x"), -2)
  test.equal(lang.execute("x = 2; y= 5 - -x; return y"), 7)
  test.equal(lang.execute("x = 2; z= 3 - -(5 - -x); return z"), 10)
end

test.unary_not = function()
  test.equal(lang.execute("return !5"), 0)
  test.equal(lang.execute("return !-5"), 0)
  test.equal(lang.execute("return !0"), 1)
  test.equal(lang.execute("x = 5; return !x"), 0)
  test.equal(lang.execute("x = -5; return !x"), 0)
  test.equal(lang.execute("x = 0; return !x"), 1)
  test.equal(lang.execute("x = 5; return !(x + 1)"), 0)
  test.equal(lang.execute("x = -5; return !(x + 1)"), 0)
  test.equal(lang.execute("x = -1; return !(x + 1)"), 1)
end

test.variable_with_underscore = function()
  test.equal(lang.execute("x_ = 5; return x_"), 5)
end

test.variable_with_special_syntax = function()
  test.equal(lang.execute("§име_$&@#!~?|_променлива = 123; return §име_$&@#!~?|_променлива"),  123)
end

test.variable_used_before_defined = function()
  test.error_raised(function () lang.execute("return x", false) end, "unknown variable 'x'")
end

test.empty_statements = function()
  test.equal(lang.execute(""), 0)
  test.equal(lang.execute(";;"), 0)
  test.equal(lang.execute(";;x=5;;;return x"), 5)
end

test.syntax_error = function()
  local codeWithSyntaxError = "a = 10 + 4;\nb = a * a - 10;\nc = (a/b;\nreturn c"
  test.error_raised(function () lang.execute(codeWithSyntaxError, false) end, "Syntax error on line 3:\n\tc = (a/b;\n\t--------^")
end

test.line_comment = function()
  local codeWithLineComment = "a = 10 + 4; # line comment \nb = a * a - 10;\nc = a/b;\nreturn c"
  test.almost_equal(lang.execute(codeWithLineComment), 0.075268817204301, 0.000000000000001)
end

test.block_comment = function()
  local codeWithLineComment = "a = 10 + 4; #{ block comment \n a line in the block\n end of block comment followed by statement #} b = a * a - 10;\nc = a/b;\nreturn c"
  test.almost_equal(lang.execute(codeWithLineComment), 0.075268817204301, 0.000000000000001)
end

test.summary()
