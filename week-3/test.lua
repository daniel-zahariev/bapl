package.path = "../utils/?.lua;" .. package.path
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
  test.error_raised(function () lang.execute("return x") end, "unknown variable 'x'")
end

test.empty_statements = function()
  test.equal(lang.execute(""), 0)
  test.equal(lang.execute(";;"), 0)
  test.equal(lang.execute(";;x=5;;;"), 0)
end
test.summary()
