local lpeg = require "lpeg"
local pt = require "pt"

-- empty function
-- test = function()end

-- done
-- > unary operators: +, -, !
-- > print with @
-- > empty statement and empty input are valid
-- > variable names can contain _
-- > variable names can contain special symbols
-- > error messages with line numbers
-- > block comments

-- misc
-- > variable names can not be used before they are defined

-- todo
-- Adding more operators - part 1: https://cdn.classpert.com/uploads/5a3229af179763171875a092d5f3638b-919481.pdf
-- Adding more operators - part 2: https://cdn.classpert.com/uploads/4311e3c061c24be4e1b5ea11c9027697-146445.pdf



----------------------------------------------------
local function I (msg)
  return lpeg.P(function () print (msg); return true end)
end

----------------------------------------------------
local function nodeNum (num)
  return {tag = "number", val = tonumber(num)}
end

local function nodeVar (var)
  return {tag = "variable", var = var}
end

local function nodeAssgn (id, exp)
  return {tag = "assgn", id = id, exp = exp}
end

local function nodePrint (op, exp)
  return {tag = "print", exp = exp}
end

local function nodeRet (exp)
  return {tag = "ret", exp = exp}
end

local function nodeEmpty ()
  return {tag = "empty"}
end

local function nodeSeq (st1, st2)
  if st2 == nil then
    return st1
  else
    return {tag = "seq", st1 = st1, st2 = st2}
  end
end

local function packUn(op, exp)
  return { tag = "unop", op = op, exp = exp }
end

local alpha = lpeg.R("AZ", "az")
local digit = lpeg.R("09")
local alphanum = alpha + digit + lpeg.S("_")

local bulgarian = lpeg.utfR(0x0430, 0x044F)
local noOp = lpeg.S("$&@#!~?|_")
local alphanumExtra = alpha + bulgarian + digit + noOp
local sigil = lpeg.P("§")

local maxMatch = 0
local maxLine = 1
local maxLineStart = 0
local function storeMaxLine(_, p) 
  if p > maxLineStart then
    maxLine = maxLine + 1
    maxLineStart = p
  end
  return true
end

local newline = lpeg.S("\r\n") * storeMaxLine
local lineComment = lpeg.P("#") * (1 - newline)^0 * newline^-1
local blockComment = lpeg.P("#{") * (-lpeg.P("#}") * (newline + 1))^0 * lpeg.P("#}")
local comment = blockComment + lineComment
local space = lpeg.V"space"

local numeral = lpeg.R("09")^1 / nodeNum  * space

local reserved = {"return",  "if"}
local excluded = lpeg.P(false)
for i = 1, #reserved do
  excluded = excluded + reserved [i]
end
excluded = excluded * -alphanum 

local ID = ((lpeg.C(alpha * alphanum^0) + lpeg.C(sigil * alphanumExtra^1)) - excluded) * space
local var = ID / nodeVar

local function T (t)
  return t * space
end

local function Rw (t)
  assert(excluded:match(t))
  return t * -alphanum * space
end

local opA = lpeg.C(lpeg.S"+-") * space
local opM = lpeg.C(lpeg.S"*/") * space
local unOp = lpeg.C(lpeg.S"+-!") * space
local opPr = lpeg.C(lpeg.S"@") * space


-- Convert a list {n1, "+", n2, "+", n3, ...} into a tree
-- {...{ op = "+", e1 = {op = "+", e1 = n1, n2 = n2}, e2 = n3}...}
local function foldBin (lst)
  local tree = lst[1]
  for i = 2, #lst, 2 do
    tree = { tag = "binop", e1 = tree, op = lst[i], e2 = lst[i + 1] }
  end
  return tree
end

local base = lpeg.V"base"
local factor = lpeg.V"factor"
local term = lpeg.V"term"
local exp = lpeg.V"exp"
local stat = lpeg.V"stat"
local stats = lpeg.V"stats"
local block = lpeg.V"block"

grammar = lpeg.P{"prog",
  prog = space * stats * -1,
  stats = stat * (T";" * stats)^-1 / nodeSeq,
  block = T"{" * stats * T";"^-1 * T"}",
  stat = block
       + opPr * exp / nodePrint
       + ID * T"=" * exp / nodeAssgn
       + Rw"return" * exp / nodeRet
       + T";"^-1 / nodeEmpty,
  base = numeral + T"(" * exp * T")" + var,
  factor = unOp * factor / packUn + base,
  term = lpeg.Ct(factor * (opM * factor)^0) / foldBin,
  exp = lpeg.Ct(term * (opA * term)^0) / foldBin,
  space = (lpeg.S(" \t") + comment + newline)^0 
          * lpeg.P(function (_, p) maxMatch = math.max(maxMatch, p); return true end),
}

local function syntaxError(input, maxLine, maxLineStart, maxMatch)
  local errorMessage = "Syntax error on line " .. maxLine .. 
                  ":\n\t" .. input:sub(maxLineStart):match("[^\n]*") .. 
                  "\n\t" .. string.rep('-', maxMatch - maxLineStart) .. "^"
  -- io.stderr:write("Syntax error on line " .. maxLine .. ":\n")
  -- io.stderr:write("\t" .. input:sub(maxLineStart):match("[^\n]*") .. "\n")
  -- io.stderr:write("\t" .. string.rep('-', maxMatch - maxLineStart) .. "^\n")
  -- io.stderr:write(errorMessage)
  error(errorMessage)
end

local function parse (input)
  -- reset state to allow reusage (in tests)
  maxLine, maxLineStart, maxMatch = 1, 0, 0

  local ast = grammar:match(input)
  if (not ast) then
    syntaxError(input, maxLine, maxLineStart, maxMatch)
    -- the syntax error throws so we don't need to exit here
    -- also we want to allow multiple calls to 'execute' function
    -- os.exit(1)
  end

  return ast
end

----------------------------------------------------
local Compiler = { code = {}, vars = {}, nvars = 0 }


function Compiler:addCode (op)
  local code = self.code
  code[#code + 1] = op
end


local binops = {["+"] = "add", ["-"] = "sub",
             ["*"] = "mul", ["/"] = "div"}


function Compiler:var2num (id, shouldExist)
  local num = self.vars[id]
  if not num then
    if shouldExist then
      error("unknown variable '".. id .. "'")
    end
    num = self.nvars + 1
    self.nvars = num
    self.vars[id] = num
  end
  return num
end


function Compiler:codeExp (ast)
  if ast.tag == "number" then
    Compiler:addCode("push")
    Compiler:addCode(ast.val)
  elseif ast.tag == "variable" then
    Compiler:addCode("load")
    Compiler:addCode(Compiler:var2num(ast.var, true))
  elseif ast.tag == "unop" then
    if ast.op == "-" then
      Compiler:codeExp(ast.exp)
      Compiler:addCode("neg")
    elseif ast.op == "+" then
      Compiler:codeExp(ast.exp)
    elseif ast.op == "!" then
      Compiler:codeExp(ast.exp)
      Compiler:addCode("not")
    else error("invalid tree")
    end
  elseif ast.tag == "binop" then
    Compiler:codeExp(ast.e1)
    Compiler:codeExp(ast.e2)
    Compiler:addCode(binops[ast.op])
  else error("invalid tree")
  end
end


function Compiler:codeStat (ast)
  if ast.tag == "assgn" then
    Compiler:codeExp(ast.exp)
    Compiler:addCode("store")
    Compiler:addCode(Compiler:var2num(ast.id))
  elseif ast.tag == "seq" then
    Compiler:codeStat(ast.st1)
    Compiler:codeStat(ast.st2)
  elseif ast.tag == "ret" then
    Compiler:codeExp(ast.exp)
    Compiler:addCode("ret")
  elseif ast.tag == "print" then
    Compiler:codeExp(ast.exp)
    Compiler:addCode("print")
  elseif ast.tag == "empty" then
    -- do nothing
  else error("invalid tree")
  end
end

local function compile (ast)
  -- clean up state to allow reusage of the Compiler
  Compiler.code = {};
  Compiler.vars = {};
  Compiler.nvars = 0;

  Compiler:codeStat(ast)
  Compiler:addCode("push")
  Compiler:addCode(0)
  Compiler:addCode("ret")
  return Compiler.code
end

----------------------------------------------------

local function run (code, mem, stack)
  local pc = 1
  local top = 0
  while true do
  --[[
  io.write("--> ")
  for i = 1, top do io.write(stack[i], " ") end
  io.write("\n", code[pc], "\n")
  --]]
    if code[pc] == "ret" then
      return
    elseif code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
    elseif code[pc] == "add" then
      stack[top - 1] = stack[top - 1] + stack[top]
      top = top - 1
    elseif code[pc] == "sub" then
      stack[top - 1] = stack[top - 1] - stack[top]
      top = top - 1
    elseif code[pc] == "mul" then
      stack[top - 1] = stack[top - 1] * stack[top]
      top = top - 1
    elseif code[pc] == "div" then
      stack[top - 1] = stack[top - 1] / stack[top]
      top = top - 1
    elseif code[pc] == "load" then
      pc = pc + 1
      local id = code[pc]
      top = top + 1
      stack[top] = mem[id]
    elseif code[pc] == "store" then
      pc = pc + 1
      local id = code[pc]
      mem[id] = stack[top]
      top = top - 1
    elseif code[pc] == "neg" then
      stack[top] = -1 * stack[top]
    elseif code[pc] == "print" then
      print(stack[top])
      top = top - 1
    elseif code[pc] == "not" then
      if stack[top] == 0 then
        stack[top] = 1
      else
        stack[top] = 0
      end
    else error("unknown instruction")
    end
    pc = pc + 1
  end
end

----------------------------------------------------
local function rawExecute(rawCode)
  local ast = parse(rawCode)
  local code = compile(ast)
  local stack = {}
  local mem = {}
  run(code, mem, stack)
  return stack[1]
end

local function execute(rawCode, printErrors)
  local status, result = pcall(rawExecute, rawCode)
  if status then
    return result
  else
    if printErrors == false then
      error(result)
    else 
      print(result)
    end
  end
end

return {execute=execute}
