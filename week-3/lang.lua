local lpeg = require "lpeg"
local pt = require "pt"

-- empty function
-- test = function()end

-- done
-- > unary operators: +, -, !
-- > print with @
-- > empty statement and empty input are valid


-- todo
-- Adding more operators - part 1: https://cdn.classpert.com/uploads/5a3229af179763171875a092d5f3638b-919481.pdf
-- Adding more operators - part 2: https://cdn.classpert.com/uploads/4311e3c061c24be4e1b5ea11c9027697-146445.pdf

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

local space = lpeg.S(" \t\n")^0
local numeral = lpeg.R("09")^1 / nodeNum  * space

local ID = (lpeg.C(alpha * alphanum^0) + lpeg.C(sigil * alphanumExtra^1)) * space
local var = ID / nodeVar

local Assgn = "=" * space
local SC = ";" * space

local ret = "return" * space

local OP = "(" * space
local CP = ")" * space
local OB = "{" * space
local CB = "}" * space

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

grammar = lpeg.P{"stats",
  stats = stat * (SC * stats)^-1 / nodeSeq,
  block = OB * stats * SC^-1 * CB,
  stat = block
       + opPr * exp / nodePrint
       + ID * Assgn * exp / nodeAssgn
       + ret * exp / nodeRet
       + SC^-1 / nodeEmpty,
  base = numeral + OP * exp * CP + var,
  factor = unOp * factor / packUn + base,
  term = lpeg.Ct(factor * (opM * factor)^0) / foldBin,
  exp = lpeg.Ct(term * (opA * term)^0) / foldBin,
}

grammar = space * grammar * -1

local function parse (input)
  return grammar:match(input)
end

----------------------------------------------------

local function addCode (state, op)
  local code = state.code
  code[#code + 1] = op
end


local binops = {["+"] = "add", ["-"] = "sub",
             ["*"] = "mul", ["/"] = "div"}


local function var2num (state, id, shouldExist)
  local num = state.vars[id]
  if not num then
    if shouldExist then
      error("unknown variable '".. id .. "'")
    end
    num = state.nvars + 1
    state.nvars = num
    state.vars[id] = num
  end
  return num
end


local function codeExp (state, ast)
  if ast.tag == "number" then
    addCode(state, "push")
    addCode(state, ast.val)
  elseif ast.tag == "variable" then
    addCode(state, "load")
    addCode(state, var2num(state, ast.var, true))
  elseif ast.tag == "unop" then
    if ast.op == "-" then
      codeExp(state, ast.exp)
      addCode(state, "neg")
    elseif ast.op == "+" then
      codeExp(state, ast.exp)
    elseif ast.op == "!" then
      codeExp(state, ast.exp)
      addCode(state, "not")
    else error("invalid tree")
    end
  elseif ast.tag == "binop" then
    codeExp(state, ast.e1)
    codeExp(state, ast.e2)
    addCode(state, binops[ast.op])
  else error("invalid tree")
  end
end


local function codeStat (state, ast)
  if ast.tag == "assgn" then
    codeExp(state, ast.exp)
    addCode(state, "store")
    addCode(state, var2num(state, ast.id))
  elseif ast.tag == "seq" then
    codeStat(state, ast.st1)
    codeStat(state, ast.st2)
  elseif ast.tag == "ret" then
    codeExp(state, ast.exp)
    addCode(state, "ret")
  elseif ast.tag == "print" then
    codeExp(state, ast.exp)
    addCode(state, "print")
  elseif ast.tag == "empty" then
    -- do nothing
  else error("invalid tree")
  end
end

local function compile (ast)
  local state = { code = {}, vars = {}, nvars = 0 }
  codeStat(state, ast)
  addCode(state, "push")
  addCode(state, 0)
  addCode(state, "ret")
  return state.code
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
local function execute(rawCode)
  local ast = parse(rawCode)
  local code = compile(ast)
  local stack = {}
  local mem = {}
  run(code, mem, stack)
  return stack[1]
end

return {execute=execute}
