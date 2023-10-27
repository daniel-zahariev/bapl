package.path = "../utils/?.lua;" .. package.path
local test = require "u-test" -- https://github.com/IUdalov/u-test
local lang = require "lang"

-- unary plus is curretly treated as a noop
test.unary_plus = function()
  test.equal(lang.execute("function main() { return +1 }"), 1)
  test.equal(lang.execute("function main() { x=12 + +1; return x }"), 13)
  test.equal(lang.execute("function main() { x=12 + ++++1; return x }"), 13)
  -- shouldn't modify negative values
  test.equal(lang.execute("function main() { x=-5; return +x }"), -5)
end

-- unary minus should apply to numbers, variables, and expressions
test.unary_minus = function()
  test.equal(lang.execute("function main() { return -1 }"), -1)
  test.equal(lang.execute("function main() { x = -5; return x }"), -5)
  test.equal(lang.execute("function main() { x = 2; return -x }"), -2)
  test.equal(lang.execute("function main() { x = 2; y= 5 - -x; return y }"), 7)
  test.equal(lang.execute("function main() { x = 2; z= 3 - -(5 - -x); return z }"), 10)
end

test.variable_with_underscore = function()
  test.equal(lang.execute("function main() { x_ = 5; return x_ }"), 5)
end


test.variable_with_special_syntax = function()
  local code = [[
    function main() {
      // the variable name is in Bulgarian
      # but any Unicode character is allowed apart from special symbols and space obviously
      §име_$&@#!~?|_променлива = 123;
      return §име_$&@#!~?|_променлива
    }
  ]]
  test.equal(lang.execute(code),  123)
end

test.variable_used_before_defined = function()
  test.error_raised(function () lang.execute("function main() { return x }", false) end, "unknown variable 'x'")
end

test.empty_statements = function()
  test.equal(lang.execute(";; function main() { return 1; } ;;x = 5;"), 1)
  test.equal(lang.execute(";;x=5;;; function main() { return x }"), 5)
end

test.syntax_error = function()
  local codeWithSyntaxError = [[
a = 10 + 4;
b = a * a - 10;
c = (a/b;
]]

  test.error_raised(function () lang.execute(codeWithSyntaxError, false) end, "Syntax error on line 3:\n\tc = (a/b;\n\t--------^")
end

test.line_comment = function()
  local codeWithLineComment = [[
    a = 10 # + 10
    # line comment a = a + 10
    a = a + 10 // + 10
    // line comment a = a + 10
    function main() { return a }
  ]]
  test.equal(lang.execute(codeWithLineComment), 20)
end

test.block_comment = function()
  local codeWithLineComment = [[
    a = 10 #{ + 10
      block comment
      a = a + 10
      another line in the block
      end of block comment is followed by 
    statement that is executed #} a = a + 10
    function main () { return a }
  ]]
  test.equal(lang.execute(codeWithLineComment), 20)
end

------------------------------------------------
-- Control stuctures
------------------------------------------------

-- Not operator
-- https://cdn.classpert.com/uploads/900dd4f469bb9b67cea0203dcea320da-138665.pdf
test.unary_not = function()
  test.equal(lang.execute("function main() { return !5 }"), 0)
  test.equal(lang.execute("function main() { return !-5 }"), 0)
  test.equal(lang.execute("function main() { return !0 }"), 1)
  test.equal(lang.execute("function main() { x = 5; return !x }"), 0)
  test.equal(lang.execute("function main() { x = -5; return !x }"), 0)
  test.equal(lang.execute("function main() { x = 0; return !x }"), 1)
  test.equal(lang.execute("function main() { x = 5; return !(x + 1) }"), 0)
  test.equal(lang.execute("function main() { x = -5; return !(x + 1) }"), 0)
  test.equal(lang.execute("function main() { x = -1; return !(x + 1) }"), 1)
end

-- Rewriting the function "node"
-- https://cdn.classpert.com/uploads/e874cb4393b998bae15cc3c031f005f7-997604.pdf
-- done
-- also added a new more elaborate function 'nodePos' which also fetches the start and end position of the matched expression

-- Relative jumps
-- https://cdn.classpert.com/uploads/40682cd06589111bef6e9080aff78016-970488.pdf
-- done

-- Elseif
-- https://cdn.classpert.com/uploads/f9b1032c15cb15b77cb484b4080b72aa-254184.pdf
test.if_elseif_else = function() 
  local mainFunc = [[
    function main () {
      if a { c = 3 }
      elseif b { c = 5 }
      else { c = 7 }
      return c
    }
  ]]
  test.equal(lang.execute("a = 1; b = 1;" .. mainFunc), 3)
  test.equal(lang.execute("a = 0; b = 1;" .. mainFunc), 5)
  test.equal(lang.execute("a = 0; b = 0;" .. mainFunc), 7)
end

test.while_loop = function() 
  local twoPow10 = [[
    a = 1;
    b = 10;
    while b { a = a * 2; b = b - 1; }
    function main () { return a }
  ]]
  test.equal(lang.execute(twoPow10), 1024)
end

-- Logical operators
-- https://cdn.classpert.com/uploads/b778ee04e388964f69d31937e870c295-700267.pdf
test.and_or = function() 
  -- and
  test.equal(lang.execute([[ function main () { return 1 and 2 } ]]), 2)
  test.equal(lang.execute([[ function main () { return -1 and 2 } ]]), 2)
  test.equal(lang.execute([[ function main () { return 0 and 2 } ]]), 0)
  test.equal(lang.execute([[ function main () { return 2 and 0 } ]]), 0)
  test.equal(lang.execute([[ function main () { return 1 and 0 and 3 } ]]), 0)
  test.equal(lang.execute([[ function main () { return 1 and 2 and 0 } ]]), 0)
  test.equal(lang.execute([[ function main () { return 1 and 2 and 3 } ]]), 3)
  -- or
  test.equal(lang.execute([[ function main () { return 1 or 2 } ]]), 1)
  test.equal(lang.execute([[ function main () { return 2 or 1 } ]]), 2)
  test.equal(lang.execute([[ function main () { return 2 or 0 } ]]), 2)
  test.equal(lang.execute([[ function main () { return 0 or 2 } ]]), 2)
  test.equal(lang.execute([[ function main () { return 1 or 0 or 3 } ]]), 1)
  test.equal(lang.execute([[ function main () { return 3 or 0 or 1 } ]]), 3)
  test.equal(lang.execute([[ function main () { return 0 or 1 or 3 } ]]), 1)
end

------------------------------------------------
-- Arrays
------------------------------------------------

-- Printing arrays
-- https://cdn.classpert.com/uploads/dfeb5aaf409b43df8b9589a83d711dbe-954205.pdf
test.print_array = function()
  local printArray = [[
    a = new [2][2]
    a[1][1] = 1
    a[1][2] = 5
    a[2][1] = 15
    a[2][2] = 25
    function main () { @ a }
  ]]
  test.equal(lang.execute(printArray), 0)
end

-- Loops in arrays
-- https://cdn.classpert.com/uploads/8463181813b9e2124958e048a8372ea2-698700.pdf
test.recursive_array = function()
  local recursiveArray = [[
    a = new [2]
    a[1] = a
    a[2] = 100
    function main () { return a[1][1][1][1][1][2] }
  ]]
  test.equal(lang.execute(recursiveArray), 100)
end

-- Add "index out of range" check
-- https://cdn.classpert.com/uploads/0dc283929094ae3d9d49b9d36e2d0eb9-205671.pdf
-- not implemented yet

-- Garbage collection
-- https://cdn.classpert.com/uploads/a2fe90979a3a01ff2906a85fa7a61c5c-928817.pdf
--[[
'top' is just a pointer to a position in the stack and when it is decreased the values after the 'top' position in the stack are not removed
we can enable garbage collection by setting those values to nil
--]]


-- Multidimensional new
-- https://cdn.classpert.com/uploads/2a136d19073013c0686791ab46942461-631015.pdf
test.new_multidimensional_array = function()
  local new3dArray = [[
    a = new [3][3][3]
    a[1][1][1] = 5
    function main () { return a[1][1][1] }
  ]]
  test.equal(lang.execute(new3dArray), 5)
end

------------------------------------------------
-- Functions
------------------------------------------------

-- Forward declaration
-- https://cdn.classpert.com/uploads/6682f1e648cf8ebfc4d58bc2490db121-558819.pdf
test.forward_declaration = function()
  local code = [[
    function odd(n);
    function even(n);
    
    function main () {
      return even(n);
    }
    
    function even(n) {
      if n {
        n = n - 1;
        return odd(n);
      } else {
        return 1;
      }
    }
    
    function odd(n) {
      if n {
        n = n - 1;
        return even(n);
      } else {
        return 0;
      }
    }
  ]]

  test.equal(lang.execute("n = 10;" .. code), 1)
  test.equal(lang.execute("n = 11;" .. code), 0)


  local missingDefinitionForDeclaration = [[
    function odd();
    function main() {return 1 }
  ]]
  test.error_raised(function () lang.execute(missingDefinitionForDeclaration, false) end, "function 'odd' declared but not defined")
end

------------------------------------------------
-- Local variables and parameters
------------------------------------------------

-- Name conflicts among functions
-- https://cdn.classpert.com/uploads/be9e4be8be01d5d5da34aef878dfe2a0-549561.pdf
-- Name conflicts between functions and global variables
-- https://cdn.classpert.com/uploads/3c9f282324f5540e115d04219881d1af-828989.pdf
test.function_and_variable_name_conflicts = function()
  local sum = [[
    function sum() { return 1 }
    function main() { return 1 }
  ]]
  test.error_raised(function () lang.execute("function sum() { return 1 }" .. sum, false) end, "function 'sum' already defined")
  test.error_raised(function () lang.execute("sum = 1" .. sum, false) end, "variable 'sum' already defined")

  local sumVar = [[
    function sum() { return 1 }
    sum = 5
    function main() { return 1 }
  ]]
  test.error_raised(function () lang.execute(sumVar, false) end, "function 'sum' cannot be used as a variable")
end

test.function_redeclaration_error = function()
  local sumRedeclared = [[
    function sum();
    function sum();
    function main() { return sum() }
    function sum() { return 1 }
  ]]
  test.error_raised(function () lang.execute(sumRedeclared, false) end, "function 'sum' already declared")
end

test.function_declaration_params_count_mismatch_error = function()
  local sumRedeclared = [[
    function sum(a, b);
    function main() { return sum(1, 2) }
    function sum(a) { return a }
  ]]
  test.error_raised(function () lang.execute(sumRedeclared, false) end, "function 'sum' declared with 2 parameters, but defined with 1")
end

-- Check for 'main'
-- https://cdn.classpert.com/uploads/eac1fe1b95eb1180986212fd6deadec0-999936.pdf
test.no_main_error = function()
  local noMainFunction = [[
    function sum(a, b) {
      return a + b
    }
  ]]
  test.error_raised(function () lang.execute(noMainFunction, false) end, "function 'main' not found")
end

test.missing_function_arguments_error = function()
  local wrongNumberOfArguments = [[
    function sum(a, b) {
      return a + b
    }
    function main() {
      return sum(1)
    }
  ]]
  test.error_raised(function () lang.execute(wrongNumberOfArguments, false) end, "wrong number of arguments for function 'sum'")
end

-- Checking redeclaration of local variables
-- https://cdn.classpert.com/uploads/872c0170fb4997772764a55a8fd9a365-463450.pdf
-- Checking redeclaration of local variables and parameters
-- https://cdn.classpert.com/uploads/8696c0fa111ccd8b95568cafb277f9d7-286293.pdf
test.redeclaration_errors = function()
  local redeclarationOfLocalVariable = [[
    function weird() {
      var x = 1
      var x = 2
      return x
    }
    function main() {
      return weird()
    }
  ]]
  test.error_raised(function () lang.execute(redeclarationOfLocalVariable, false) end, "variable 'x' already defined")
  
  local redeclarationOfParameter = [[
    function sum(a, a) {
      return a + a
    }
    function main() {
      return sum(1, 1);
    }
  ]]
  test.error_raised(function () lang.execute(redeclarationOfParameter, false) end, "duplicate parameter 'a'")

  -- in order to allow block shadowing we allow redeclaration of local variables even when they appear as parameters first
  -- in the most outer function block scope this effectively overwrites the parameter with the local variable
  local redeclarationOfParameterAsVariable = [[
    function sum(a, b) {
      var a = 10;
      { var a = 100; }
      return a + b
    }
    function main() {
      return sum(1, 1);
    }
  ]]
  test.equal(lang.execute(redeclarationOfParameterAsVariable, false), 11)

end

-- Default arguments
-- https://cdn.classpert.com/uploads/4d03e5377280eb0d3fa3a0be7621c85a-709820.pdf
-- any number of arguments can have default values
test.default_arguments = function()
  local sumWithDefaults = [[
    function sum(a, b = 5, c = 10) {
      return a + b + c
    }
  ]]
  test.error_raised(
    function () lang.execute(sumWithDefaults .. "function main() { return sum() }", false) end,
    "wrong number of arguments for function 'sum'"
  )
  test.equal(lang.execute(sumWithDefaults .. " function main() { return sum(1) } ", false), 16)
  test.equal(lang.execute(sumWithDefaults .. " function main() { return sum(1, 1) } ", false), 12)
  test.equal(lang.execute(sumWithDefaults .. " function main() { return sum(1, 1, 1) } ", false), 3)

  local moreElaborateDefaults = [[
    base = 100
    function getDefaultValue(n = 1) {
      return base + n
    }
    // default values can be expressions
    function getConfigValue(a = getDefaultValue(10), b = getDefaultValue()) {
      return a + b
    }
    function main() {
      var config1 = getConfigValue() // resolves to 211
      base = 200
      var config2 = getConfigValue(0) // resolves to 201
      return config1 + config2
    }
  ]]
  test.equal(lang.execute(moreElaborateDefaults, false), 412)
end

test.summary()
