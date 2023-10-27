package.path = "../utils/?.lua;" .. package.path
local lpeg = require "lpeg"
local pt = require "pt"

----------------------------------------------------
local function I (msg)
  return lpeg.P(function () print (msg); return true end)
end

local function node (tag, ...)
  local labels = table.pack(...)
  return function (...)
    local params = table.pack(...)
    local tbl = {tag = tag}
    for i = 1, #labels do
      tbl[labels[i]] = params[i]
    end
    return tbl
  end
end
----------------------------------------------------

local function nodeSeq (st1, st2)
  if st2 == nil then
    return st1
  else
    return {tag = "seq", st1 = st1, st2 = st2}
  end
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

local numeral = lpeg.R("09")^1 / tonumber / node("number", "val")  * space

local reserved = {"return", "if", "elseif", "else", "while", "and", "or", "new", "function"}
local excluded = lpeg.P(false)
for i = 1, #reserved do
  excluded = excluded + reserved [i]
end
excluded = excluded * -alphanum 

local ID = lpeg.V"ID"
local var = ID / node("variable", "var")

local function T (t)
  return t * space
end

local function Rw (t)
  assert(excluded:match(t))
  return t * -alphanum * space
end

local opE = lpeg.C(Rw"and" + Rw"or") * space
local opA = lpeg.C(lpeg.S"+-") * space
local opM = lpeg.C(lpeg.S"*/") * space
local unOp = lpeg.C(lpeg.S"+-!") * space
local opPr = lpeg.S"@" * space


-- Convert a list {n1, "+", n2, "+", n3, ...} into a tree
-- {...{ op = "+", e1 = {op = "+", e1 = n1, n2 = n2}, e2 = n3}...}
local function foldBin (lst)
  local tree = lst[1]
  for i = 2, #lst, 2 do
    tree = { tag = "binop", e1 = tree, op = lst[i], e2 = lst[i + 1] }
  end
  return tree
end

local function foldIndex(lst)
  local tree = lst[1]
  for i = 2, #lst do
    tree = { tag = "indexed", array = tree, index = lst[i] }
  end
  return tree
end

local lhs = lpeg.V"lhs"
local call = lpeg.V"call"
local newA = lpeg.V"newA"
local base = lpeg.V"base"
local factor = lpeg.V"factor"
local term = lpeg.V"term"
local expMD = lpeg.V"expMD"
local exp = lpeg.V"exp"
local elif = lpeg.V"elif"
local stat = lpeg.V"stat"
local stats = lpeg.V"stats"
local block = lpeg.V"block"
local funcDec = lpeg.V"funcDec"

grammar = lpeg.P{"prog",
  prog = space * lpeg.Ct(funcDec^1) * -1,

  funcDec = Rw"function" * ID * T"(" * T")" * (block + T";") / node("function", "name", "body"),
  
  call = ID * T"(" * T")" / node("call", "fname"),

  stats = stat * (T";" * stats)^-1 / nodeSeq,

  block = T"{" * stats * T";"^-1 * T"}" / node("block", "body"),

  stat = block
       + opPr * exp / node("print", "exp")
       + Rw"while" * exp * block / node("while", "cond", "body")
       + Rw"if" * exp * block * (elif + (Rw"else" * block)^-1) / node("if", "cond", "th", "els")
       + call
       + lhs * T"=" * exp / node("assgn", "lhs", "exp")
       + Rw"return" * exp / node("ret", "exp")
       + T";"^-1 / node("empty"),
  
  elif = Rw"elseif" * exp * block * (elif + Rw"else" * block)^-1 / node("if", "cond", "th", "els"),
  
  lhs = lpeg.Ct(var * (T"[" * exp * T"]")^0) / foldIndex,
  
  newA = T"[" * exp * T"]" * newA^-1 / node("new", "size", "element"),
  
  base = Rw"new" * newA
        + numeral
        + T"(" * exp * T")"
        + call
        + lhs,
  
  factor = unOp * factor / node("unop", "op", "exp") + base,
  
  term = lpeg.Ct(factor * (opM * factor)^0) / foldBin, -- +-
  
  expMD = lpeg.Ct(term * (opA * term)^0) / foldBin, -- */
  exp = lpeg.Ct(expMD * (opE * expMD)^0) / foldBin, -- and or
  space = (lpeg.S(" \t") + comment + newline)^0 
          * lpeg.P(function (_, p) maxMatch = math.max(maxMatch, p); return true end),
  ID = ((lpeg.C(alpha * alphanum^0) + lpeg.C(sigil * alphanumExtra^1)) - excluded) * space
}

local function syntaxError(input, maxLine, maxLineStart, maxMatch)
  local errorMessage = "Syntax error on line " .. maxLine .. 
                  ":\n\t" .. input:sub(maxLineStart):match("[^\n]*") .. 
                  "\n\t" .. string.rep('-', maxMatch - maxLineStart) .. "^"
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
local Compiler = { funcs = {}, vars = {}, nvars = 0 }


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

function Compiler:currentPosition ()
  return #self.code  
end

function Compiler:codeJmpRel (op, target)
  if target == nil then
    target = self:currentPosition()
  end
  self:addCode(op)
  self:addCode(target - self:currentPosition() - 1)
  return self:currentPosition()
end

function Compiler:fixJmpRel (jmp, target)
  if target == nil then
    target = self:currentPosition()
  end
  self.code[jmp] = target - jmp
end

function Compiler:codeCall (ast)
  local func = self.funcs[ast.fname]
  if not func then
    error("unknown function '" .. ast.fname .. "'")
  end

  self:addCode("call")
  self:addCode(func.code)
end

function Compiler:codeExp (ast)
  if ast.tag == "number" then
    self:addCode("push")
    self:addCode(ast.val)
  elseif ast.tag == "call" then
    self:codeCall(ast)
  elseif ast.tag == "variable" then
    self:addCode("load")
    self:addCode(self:var2num(ast.var, true))
  elseif ast.tag == "indexed" then
    self:codeExp(ast.array)
    self:codeExp(ast.index)
    self:addCode("getarray")
  elseif ast.tag == "new" then
    self:codeExp(ast.size)
    if ast.element then
      self:codeExp(ast.element)
    else
      self:addCode("pushNil")
    end
    self:addCode("newarray")
  elseif ast.tag == "unop" then
    if ast.op == "-" then
      self:codeExp(ast.exp)
      self:addCode("neg")
    elseif ast.op == "+" then
      self:codeExp(ast.exp)
    elseif ast.op == "!" then
      self:codeExp(ast.exp)
      self:addCode("not")
    else error("invalid tree")
    end
  elseif ast.tag == "binop" then
    -- TODO the capture of and/or inclues the space as it is part of the Rw pattern
    local op = string.gsub(ast.op, "%s+", "")
    -- print(op)
    -- print(pt.pt(ast))
    if op == "and" then
      self:codeExp(ast.e1)
      local jmp = self:codeJmpRel("jmpZP")
      self:codeExp(ast.e2)
      self:fixJmpRel(jmp)
    elseif op == "or" then
      self:codeExp(ast.e1)
      local jmp = self:codeJmpRel("jmpNZP")
      self:codeExp(ast.e2)
      self:fixJmpRel(jmp)
    else
      self:codeExp(ast.e1)
      self:codeExp(ast.e2)
      self:addCode(binops[ast.op])
    end
  else error("invalid tree")
  end
end


function Compiler:codeAssgn (ast)
  local lhs = ast.lhs
  if lhs.tag == "variable" then
    self:codeExp(ast.exp)
    self:addCode("store")
    self:addCode(self:var2num(lhs.var))
  elseif lhs.tag == "indexed" then
    self:codeExp(lhs.array)
    self:codeExp(lhs.index)
    self:codeExp(ast.exp)
    self:addCode("setarray")
  else error("unknown lhs")
  end

end

function Compiler:codeBlock (ast)
  self:codeStat(ast.body)
end

function Compiler:codeStat (ast)
  if ast.tag == "assgn" then
    self:codeAssgn(ast)
  elseif ast.tag == "call" then
    self:codeCall(ast)
    self:addCode("pop")
    self:addCode(1)
  elseif ast.tag == "block" then
    self:codeBlock(ast)
  elseif ast.tag == "seq" then
    self:codeStat(ast.st1)
    self:codeStat(ast.st2)
  elseif ast.tag == "while" then
    local start = self:currentPosition()
    self:codeExp(ast.cond)
    local jmp = self:codeJmpRel("jmpZ")
    self:codeStat(ast.body)
    self:codeJmpRel("jmp", start)
    self:fixJmpRel(jmp)
  elseif ast.tag == "if" then
    self:codeExp(ast.cond)
    local jmp = self:codeJmpRel("jmpZ")
    self:codeStat(ast.th)
    if ast.els then
      local jmp2 = self:codeJmpRel("jmp")
      self:fixJmpRel(jmp)
      self:codeStat(ast.els)
      self:fixJmpRel(jmp2)
    else
      self:fixJmpRel(jmp)
    end
  elseif ast.tag == "ret" then
    self:codeExp(ast.exp)
    self:addCode("ret")
  elseif ast.tag == "print" then
    self:codeExp(ast.exp)
    self:addCode("print")
  elseif ast.tag == "empty" then
    -- do nothing
  else error("invalid tree")
  end
end

function Compiler:codeFunction (ast)
  if self.vars[ast.name] then
    error("variable '" .. ast.name .. "' already defined")
  end
  local func = self.funcs[ast.name]
  if func ~= nil and func.onlyDeclared ~= true then
    error("function '" .. ast.name .. "' already defined")
  end
  
  local code = {}
  -- forward declaration
  if not ast.body then
    self.funcs[ast.name] = { code = code, onlyDeclared = true}
    return
  elseif func ~= nil and func.onlyDeclared then
    self.funcs[ast.name].onlyDeclared = nil
  else
    self.funcs[ast.name] = { code = code }
  end
  self.code = self.funcs[ast.name].code
  self:codeStat(ast.body)
  self:addCode("push")
  self:addCode(0)
  self:addCode("ret")
end

local function compile (ast)
  -- clean up state to allow reusage of the Compiler
  Compiler.code = {};
  Compiler.funcs = {};
  Compiler.vars = {};
  Compiler.nvars = 0;

  for i = 1, #ast do
    Compiler:codeFunction(ast[i])
  end

  local main = Compiler.funcs["main"];
  if not main then
    error("no function 'main'")
  end
  
  return Compiler.code
end

----------------------------------------------------

local function run (code, mem, stack, top)
  local pc = 1
  while true do
  --[[
  io.write("--> ")
  for i = 1, top do io.write(stack[i], " ") end
  io.write("\n", code[pc], "\n")
  --]]
    if code[pc] == "ret" then
      return top
    elseif code[pc] == "call" then
      pc = pc + 1
      local code = code[pc]
      top = run(code, mem, stack, top)
    elseif code[pc] == "pop" then
      pc = pc + 1
      top = top - code[pc]
    elseif code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
    elseif code[pc] == "pushNil" then
      top = top + 1
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
    elseif code[pc] == "newarray" then
      local size = stack[top - 1]
      local element = stack[top]
      local arr = { size = size }
      if element ~= nil then
        for i = 1, size do
          local clone = {}
          for k, v in pairs(element) do
            clone[k] = v
          end
          arr[i] = clone
        end
      end
      stack[top - 1] = arr
      top = top - 1
    elseif code[pc] == "getarray" then
      local array = stack[top - 1]
      local index = stack[top]
      if index < 1 or index > array.size then
        error("array index out of bounds")
      end
      stack[top - 1] = array[index]
      top = top - 1
    elseif code[pc] == "setarray" then
      local array = stack[top - 2]
      local index = stack[top - 1]
      local value = stack[top]
      if index < 1 or index > array.size then
        error("array index out of bounds")
      end
      array[index] = value
      top = top - 3
    elseif code[pc] == "jmp" then
      pc = pc + 1
      pc = pc + code[pc]
    elseif code[pc] == "jmpZ" then
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then
        pc = pc + code[pc]
      end
      top = top - 1
    elseif code[pc] == "jmpZP" then
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then
        pc = pc + code[pc]
      else
        top = top - 1
      end
    elseif code[pc] == "jmpNZP" then
      pc = pc + 1
      if stack[top] ~= 0 and stack[top] ~= nil then
        pc = pc + code[pc]
      else
        top = top - 1
      end
    elseif code[pc] == "neg" then
      stack[top] = -1 * stack[top]
    elseif code[pc] == "print" then
      local value = stack[top]
      if type(x) == "string" then
        print("'" .. value .. "'")
      else
        print(pt.pt(value))
      end
      top = top - 1
    elseif code[pc] == "not" then
      if stack[top] == 0 then
        stack[top] = 1
      else
        stack[top] = 0
      end
    else error("unknown instruction: " .. code[pc])
    end
    pc = pc + 1
  end
end

----------------------------------------------------
local function rawExecute(rawCode)
  local ast = parse(rawCode)
  print(pt.pt(ast))
  print('------------------------------')
  local code = compile(ast)
  print(pt.pt(code))
  print('------------------------------')
  local stack = {}
  local mem = {}
  run(code, mem, stack, 0)
  
  print(pt.pt(stack))
  print('------------------------------')
  return stack[1]
  -- return 0
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
