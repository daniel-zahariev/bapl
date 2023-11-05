# Building a Programming Language course

The directory `week-9` contains the final version of the language presented in the class lectures, but with some improvements as well.

# Goals

My goal was to understand the steps that involve creating a programming language starting with its grammar, deciding whether the parsing will be one-step or multistep and then continuing with compiling for a target virtual machine and executing it. I don't have an immediate need to create a DSL, but this course gave me very good base to be able to create one if i needed to.

# Learning

- It was joyful experience to learn the intricacies of lexers and how AST is constructed from the source code
- Realised that some design choices are forced because of the single-pass parsing - like forward declarations
- It was very interesting to see how compilation needs resulted in VM updates and that at the same time VM constrains the compilation and forces it to be very thoughtful about resource usage
- much more appreciate the dilemma global-first vs local-first and all the consequences for a language following the decision on this question

# Elegance

- Managed to make the the statement separator (semicolon) optional which i very much like in Lua

# Experiments

- Managed to implement variable names with UTF-8 & special characters

# Aiming for the Sky Goals

- add more types
- add type system
- lambda functions
- import/export mechanisms

# Safety

- Type checks
- out of range array access check
- function & variable name clashes

# Language Features

- the langage allows any king of statement and function declaration & definition on source file level
- the language requires `main` function that has no parameters
- a function can have any number of required parameters, followed by any number of optional parameters that have a default value
- a default parameter of a function can take any valid expression as a default value, including a function call (which is evaluated at the time of the function call)
- function checks and limitations:

  - a function that is declared requires definition
  - a function declaration cannot follow the definition
  - as empty blocks are not allowed, thus empty functions are not allowed
  - `return` is not required, and where it is missing the function will evaluate to `0` (number)

- local variables are defined with `var` whereas global ones just need to be assigned to
- static-size arrays are supported with index starting from 1 and initialized with `new` reserved keyword
- multidimensional arrays are supported including array loop
- Operations

  - Arithmetic operations: +, -, /, \*, %, ^
  - Comparison operators: ==, ~=, <=, <, >=, >
  - `and` for short-cut AND
  - `or` for short-cut OR
  - `!` for NOT
  - unary plus or minus.

- Control structures

  - `if / elseif / else`
  - `while`

- Reserved keywords: `return`, `if`, `elseif`, `else`, `while`, `and`, `or`, `new`, `function`, `var`
- parse & compile errors, which point to the problematic line

# Final Project

The directory `week-9` holds the following files:

- `lang.lua` - language definition
- `run.lua` - a file for interaction with the language in the console
- `test.lua` - a test suite for the language
