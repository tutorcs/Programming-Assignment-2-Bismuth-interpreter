# Programming Assignment 2: Bismuth interpreter

**See Canvas for the due date**

In this assignment, you are going to write an interpreter for an _external_
domain specific language for image manipulation: Bismuth.  The goals of this
assignment are:

- Seeing the difference between embedded vs external DSLs,
- Recursively traversing ASTs,
- Following a formal specification using inference rules, and
- Dealing with scopes and environments.

In the end, you are going to implement a small embeded domain-specific language
(DSL) in Haskell for creating and manipulating images declaratively.  In
assignment 2, you are going to build on this assignment to create an external
DSL that others can use to create images without any Haskell knowledge.

**Additionally, there are two extra credit portions for this assignment, which
are discussed at the end of this document.**

**There are a lot of ungraded tests, so read the grading portion.**

You are going to re-use your implementation of functional images (a1part2).
Apart from these, You only need to edit `src/Eval.hs`. I will ignore changes to
other files when grading your assignment, and I will test your interpreter using
my solution to a1part2.

You should copy your implementation of specific functions in `Color.hs`,
`Point.hs`, and `Image.hs` from your solution to a1part2.  Once I release my
solution to those, you can copy my solution (which contains another function
`scaleIm2` that you will need in assignment). **Do not copy whole files because
there are other functions that have changed between assignments!**

Overall, you need to write ~50 lines of code.  My implementation took 40 lines
of code (including the extra credit parts).

## Directory structure

This is the Stack directory structure we will use in programming assignments:

- `src/` directory contains the actual implementation (the library code).
  - `Syntax.hs` contains the definition of the AST.
  - `Domains.hs` contains the semantic domains (the objects that Bismuth
    programs interact with).
  - `Parser.hs` contains the parser, you can ignore this.
  - `Eval.hs` contains the actual evaluation function (the interpreter you will
    implement).
- `app/` directory contains the command-line program that invokes the
  interpreter.
- `test/` directory contains the test code.  Your grade will be based on how
  many tests your program passes.  Each module in `src/` directory has a
  corresponding module in the `test/` directory specifying what that module is
  ought to do.  For example, the tests for the `Color` module are in
  `ColorSpec` module.
- `handwritten/` directory contains some example Bismuth programs that should
  work once your interpreter is complete.
- `handwritten-failure/` directory contains some example Bismuth programs that
  **should** cause runtime errors.
- `handwritten-output/` directory contains the output of the example programs.
- All files in the root directory (The `<project name>.cabal` file,
  `package.yaml`, `stack.yaml`, `Setup.hs`) are there to tell Stack
  how to build the project.  You can ignore those files completely.
- Similarly, you can ignore the `shell.nix` file.  I use it to create the
  sandbox to run your submissions later.

**Do not delete or change any of the files in the root of the project directory,
unless I explicitly told you so.  That may cause your project not to build.**

## Bismuth syntax

Here is the abstract syntax of Bismuth, some gotchas about the concrete syntax
are mentioned in the next section.  All of the abstract syntax cases here are
defined in the `Syntax` module.  Some cases have a name or explanation (written
in the style of Haskell comments).

```
color ∈ Color
ξ ∈ Var                                 -- variables
d ∈ ℝ                                   -- these are doubles in the implementation

-- expressions denoting images
e ∈ Expr ::= color | d | true | false   -- literals denoting values
           | x
           | y
           | r
           | θ
           | ξ
           | e₁ ⊕ e₂                    -- binary operations
           | ⊞ e                        -- unary operations
           | ξ = e₁; e₂                 -- local variable definition (looks like assignment, but isn't!)
           | [e₁, e₂, e₃, e₄]           -- color expressions
           | flip(e)
           | scale(a, e)
           | swirl(a, e)
           | translate(a₁, a₂, e)
           | scale(a₁, a₂, e)           -- scale by different ratios on the x and y axes
           | rotate(a, e)
           | overlay(e₁, e₂)
           | if e₁ e₂ e₃
           | e₁ <..> e₂                 -- the <..> operator from a1part2
           | e₁ <:> e₂                  -- the <:> operator from a1part2
           -- the remaining cases are extra credit
           | juxtapose(e₁, ...)
           | replicate(a, e)

-- binary operators over images
⊕ ∈ BinOp ::= + | - | ÷ | × | < | == | ∨ | ∧ | xor | <:> | <..>
-- unary operators over images
⊞ ∈ UnOp ::= - | ¬ | sin | cos | tan | sqrt

-- these are arithmetic expressions, they evaluate to numbers, not images.
-- Also, note that there are no variables in an arithmetic expression.
a ∈ ArithExpr ::= π | a₁ ⊗ a₂ | ⊠ a | d

-- binary and unary operators over numbers
⊗ ∈ BinOpA ::= + | - | ÷ | ×
⊠ ∈ UnOpA  ::= - | sin | cos | tan | sqrt

-- a program consists of two numbers (denoting the size of the image), and an
-- expression describing the image.
program ∈ Program ::= (n, n); e

n ∈ ℕ
```

Note that I use the meta-variable ξ (xi) to denote variables in the program
because `x` is a special case (the expression returning the x coordinate).

### Concrete vs abstract syntax

In the concrete syntax, there are some conveniences you'd expect from a
programming language.  All of these are provided by the parser, and they matter
only when you are writing Bismuth programs (e.g. for the extra credit portion):

- Variable names are more-or-less the same as valid
  Haskell identifiers, however `r`, `theta`, `x`, `y`, and `pi` are reserved
  keywords among with some common color names and operator names.  See the
  definitions of `P.reservedNames` and `P.namedColors` for the whole list.
- Additionally, Bismuth supports Java-style comments (both `//...` and `/*...*/`).
- You can use curly braces (`{}`) and parentheses (`()`) to .
- You can omit alpha for color literals, it is 1.0 by default.
- Because it is difficult to type the mathematical symbols, the operators in
  concrete syntax are represented as:
  - `^` for xor
  - `&&` for `∧` (and)
  - `||` for `∨` (or)
  - `!` for `¬` (not)
  - `*` for `×`
  - `/` for `÷`
- Similarly, `theta` is used for `θ`.
- `sin`, `tan`, `cos`, `sqrt` require parentheses after them, to make them look
  more Java-like.
- Also, the concrete syntax supports `>`, `e₁ > e₂` is parsed as `e₂ < e₁`.
- The true and false branches of an `if` expression always need curly braces
  (like the homework 3 grammar, or Rust). So, the concrete syntax for an if
  expression is: `if e₁ { e₂ } else { e₃ }`.z
- The two versions of scale are called `Scale` and `Scale2` in the abstract data
  type cases, but both are written as `scale` in the concrete syntax.

## Semantic domains

The only domains relevant to the semantics of Bismuth programs are values (what
expressions are evaluated to), and environments (to keep track of local
variables' values).  Both of these are defined in the `Domains` module.  This
module also contains some helper functions to manipulate `Value` objects in
Haskell easily.

### Values

All arithmetic expressions evaluate to just numbers, so they don't get a special
representation in our discussion.  In Haskell, we just use a type alias.

As for image expressions (the `Expr` set), they evaluate to _values_, which is
just a tagged union of different kinds of images:

```
v ∈ Value = Image Color | Image Double | Image Bool
```

Each one of the cases above also has a tag in Haskell, so the Haskell data type
looks like this:

```haskell
data Value = ColorV ImageColor
           | GrayV ImageGray
           | MaskV ImageMask
```

There are several helpers in the `Domains` module:

- The `liftVPoly` function can help lift polymorphic image transformations
  (flip, scale and the like) to operate on values.
- The `ValueLike` class provides conversion methods between each image case and
  actual objects of the value type (e.g. between `Image Color` and `Value`).
- `liftV2` is useful when combining multiple images that should be of the same
  type. This function implements the necessary _dynamic type checking_ because
  Bismuth is dynamically-typed (all Haskell type checking is still static).
- `liftVs` extends this functionality to work on non-empty lists of values (it
  fails at run time with an empty list).

### Environments

Environments are just partial maps from variables to values, we use the Greek
letter ρ (rho) to denote an environment.  This is standard notation, and it
comes from a pun on an environment made up of "rows" like a table.  You can
apply all the stuff we covered about environments from the λ calculus lectures
to here.

```
ρ ∈ Env ::= Var → Value
```

Basically, an environment represents

We are going to implement environments as `Map`s in Haskell (akin to `TreeMap`
in Java).  So, you will need to use the functions from the `Data.Map.Strict`
module to access and manipulate environments.  This module is imported as `M`,
and the indexing operator `!?` is imported directly, so you can use them in
`Eval` already.

When accessing an environment, we will write things like `ρ(ξ) = v`, this
statement means that `ρ` maps `ξ` to `v`.  Depending on where it occurs, it may
mean like a look-up.  For example, if we have `ρ = [a ↦ red, b ↦ blue]`, then
`ρ(a) = red`.  Here, `red` and `blue` are images, and `a` and `b` are the local
variables that are currently in scope.

Additionally, we will use the notation `ρ[ξ ↦ v]` to create a map that is same
as `ρ` but maps `ξ` to `v`.
Using the same example, `ρ[c ↦ green] = [a ↦ red, b ↦ blue, c ↦ green]` and
`ρ[a ↦ yellow] = [a ↦ yellow, b ↦ blue]`.

## Meaning of each expression case

Here, we are going to use the basic judgment `ρ ⊨ e ⇓ v` to mean "Under the
environment `ρ`, `e` evaluates to `v`". Additionally, we will use the following
notation (you will use eval family of functions to do these operations):

- 〚a〛 means "evaluate the arithmetic expression a".
- 〚⊕〛 and 〚⊞〛 means "the function performing the operation ⊕ or ⊞".

Also,
- "e₁, ..." means a list of expressions starting with e₁ (we already used this
  when describing the syntax).
- "rotate(a, e)" is abstract syntax, "rotate♯ 〚a〛 v" denotes lifting and
  evaluating the rotate function from the functional images library (at the
  Haskell level).  The same applies to `scale`, `translate`, etc.
- In general, `f#` will denote the Haskell function `f` (possibly lifted).
- Because all of the rules below are in ASCII, we will write each premise on its
  own line to save on horizontal space and readability.
  
The first 7 rules are already implemented for you:

```
------------------- lit-color
ρ ⊨ color ⇓ color#

------------------- lit-gray
ρ ⊨ d ⇓ d#

b ∈ {true, false}
------------------- lit-bool
ρ ⊨ b ⇓ b#

------------------- coord-x
ρ ⊨ x ⇓ getX#

------------------- coord-y
ρ ⊨ y ⇓ getY#

----------------------------- coord-r
ρ ⊨ r ⇓ (\Polar r _ -> r)#

----------------------------- coord-θ
ρ ⊨ θ ⇓ (\Polar _ θ -> θ)#

ρ ⊨ e₁ ⇓ v₁
ρ ⊨ e₂ ⇓ v₂
ρ ⊨ e₃ ⇓ v₃
ρ ⊨ e₄ ⇓ v₄
v₁, v₂, v₃ and v₄ are all grayscale images
-------------------------------------------- color-e
ρ ⊨ [e₁, e₂, e₃, e₄] ⇓ mkColor# v₁ v₂ v₃ v₄
```

Note that the only recursive rule here is the `color-e` rule: it is the only
rule with premises of the form `... ⊨ ... ⇓ ...`.  None of the other rules are
recursive (none of them refer to the same judgment in both the conclusion and in
the premises).

The following rule is also not recursive, and describes how to
look up variables:

```
ρ(ξ) = v
----------- var
ρ ⊨ ξ ⇓ v
```

Together with the following rule, they describe how scoping works:

```
ρ ⊨ e₁ ⇓ v₁
ρ[ξ ↦ v₁] ⊨ e₂ ⇓ v
---------------------- let
ρ ⊨ (ξ = e₁; e₂) ⇓ v
```

Here, `let` is our first recursive rule. It reads as:

- In order to compute what `ξ = e₁; e₂` evaluates to:
  1. Evaluate `e₁` under the current environment, and recall its value (`v₁`)
  2. Evaluate `e₂` under the current environment with `ξ` bound to `v₁`. The
     result of this evaluation is the result we are looking for.

Binary and unary operators basically lift the relevant operations, the error
handling for them is already handled for you in the helpers:

```
ρ ⊨ e₁ ⇓ v₁
ρ ⊨ e₂ ⇓ v₂
v₁ 〚⊕〛 v₂ = v
----------------- binop
ρ ⊨ e₁ ⊕ e₂ ⇓ v

ρ ⊨ e' ⇓ v'
〚⊞〛 v' = v
----------------- unop
ρ ⊨ ⊞ e' ⇓ v
```

Note that the last premise of each of these rules tells us to "just evaluate the
operation on the result of evaluating the subexpressions".

The next set of rules work more or less the same as each other (modulo the
number of arithmetic expressions.  You need to make sure to lift the relevant
functions properly.

```

ρ ⊨ e ⇓ v
----------------------------------- flip
ρ ⊨ flip(e) ⇓ flipV# v

ρ ⊨ e ⇓ v
----------------------------------- rotate
ρ ⊨ rotate(a, e) ⇓ rotateIm# 〚a〛 v

ρ ⊨ e ⇓ v
----------------------------------- swirl
ρ ⊨ swirl(a, e) ⇓ swirlIm# 〚a〛 v

ρ ⊨ e ⇓ v
----------------------------------- scale
ρ ⊨ scale(a, e) ⇓ scaleIm# 〚a〛 v

ρ ⊨ e ⇓ v
----------------------------------------------- scale2
ρ ⊨ scale(a₁, a₂, e) ⇓ scaleIm2# 〚a₁〛 〚a₂〛 v

ρ ⊨ e ⇓ v
----------------------------------------------------------------------- translate
ρ ⊨ translate(a₁, a₂, e) ⇓ translateIm# (Vector 〚a₁〛 〚a₂〛) v
```

In the following set of expressions, you need to combine multiple expressions'
results using lift functions. Here, you need to do some error handling (making
sure that the values are of correct and compatible types).  The liftV family of
functions can help getting some of this error checking for free.  However, you
need to use `fromValue` or pattern matching in other cases.

```

ρ ⊨ e₁ ⇓ v₁
ρ ⊨ e₂ ⇓ v₂
v₁ and v₂ are grayscale images
------------------------------------------ overlay
ρ ⊨ overlay(e₁, e₂) ⇓ overlayImage# v₁ v₂

ρ ⊨ e₁ ⇓ v₁
ρ ⊨ e₂ ⇓ v₂
ρ ⊨ e₃ ⇓ v₃
v₁ is a mask
--------------------------------------- if
ρ ⊨ if e₁ e₂ e₃ ⇓ (select v₁)# v₂ v₃

ρ ⊨ e₁ ⇓ v₁
ρ ⊨ e₂ ⇓ v₂
v₁ and v₂ are the same type of images
--------------------------------------- juxtapose-h
ρ ⊨ e₁ <..> e₂ ⇓ v₁ <..># v₂

ρ ⊨ e₁ ⇓ v₁
ρ ⊨ e₂ ⇓ v₂
v₁ and v₂ are the same type of images
--------------------------------------- juxtapose-v
ρ ⊨ e₁ <:> e₂ ⇓ v₁ <:># v₂
```

Note: we describe `if` in terms of `select`, because the semantics for `if`
expressions in Bismuth is to be evaluated separately for each point (thus
allowing doing things like painting inside of a circle red, and outside blue).

The last two rules are extra credit:

```

ρ ⊨ e₁ ⇓ v₁
ρ ⊨ e₂ ⇓ v₂
...
------------------------------------------------------ juxtapose
ρ ⊨ juxtapose(e₁, e₂, ...) ⇓ juxtapose# [v₁, v₂, ...]

round(〚a〛) = i > 0
ρ ⊨ juxtapose(e₁, ..., eᵢ) ⇓ v
e₁ = e₂ = ... = eᵢ = e
------------------------------------------------------ replicate
ρ ⊨ replicate(a, e) ⇓ v
```

Here, `round` is the "round-to-nearest-integer" function.  It is already in the
Haskell standard library.  The second and the third lines describe that
replicate basically juxtaposes the same image `i` times.

## Helpers available to you

There are several helpers that make this assignment _much easier_ (rather than
adding lots of recursion and nested pattern matching cases):

- liftV family of functions help connect the functions from assignment 1 part 1
  to the values.  You may need to lift single values to images, then images to
  values.  Note that these functions return a `Maybe` for error handling, but
  you need to return an `Either` in `eval`, so writing a helper to convert
  between the two may come in handy.  I have created a template for such a
  helper in `Eval.hs`.
- The `err` function allows creating error cases (when you need to return a
  `Left`).
- `evalArith`, `evalBOp` and `evalUOp` already implement the actual evaluation
  of basic operations, so you don't need to implement those, you just need to
  extract the right expressions/values to pass to them.
- `mkColor` is pretty useful.
- The methods `fromValue` and `toValue` can help converting between images and
  `Value` objects (e.g., when you get an image from a Haskell function and need
  to return a `Value` wrapping it).

## What you need to implement

You need to finish the implementation of the `eval` function in the `Eval`
module, by filling in `todo` cases.  You need to evaluate each case according to
the descriptions given above.  The last two cases are for extra credit (there is
another extra credit part, see the full description under the grading section).

As you can see from the type of `eval`, it takes an environment `ρ` and an
expression `e`, and potentially produces a resulting value (so the return type
is `Either RuntimeError Value`).

Your implementation of `eval` should satisfy the following for any given `ρ` and
`e`:
- If `ρ ⊨ e ⇓ v` then `eval ρ e = Right v`.  That is, if the rules say `e`
  evaluates to `v` under `ρ`, then `eval ρ e` should return `Right v`.
- Othrwise, `eval ρ e` is a `Left` value.

When running a program, the `run` function will call the `eval` function with an
empty environment (so, it tries to compute `v` from `⊨ e ⇓ v`).

## Either and error handling

I have written a short tutorial on error handling in Haskell (under the
Functional Programming module), please read it before attempting the assignment.
Feel free to ask me or the TAs any questions you have.

## Example programs

The `handwritten/` directory contains several example Bismuth programs.  These are
also some of your test cases.

## Running your code to output the example images

`app/Main.hs` implements the main entry point of a program that uses the library
you are building.  You can run this program using:

```haskell
stack run -- bismuth-file output-file
```

You can run this program to evaluate a Bismuth program and render the resulting
image to a PNG file. For example, to evaluate the example program
`handwritten/lambda-color.bi` to a PNG file name `lambdas-are-cool.png`, you'd run:

```haskell
stack run -- handwritten/lambda-color.bi lambdas-are-cool.png
```

If you implemented all image transformers used in this example, you should see
the following image when you open `lambdas-are-cool.png`:

![The image described in handwritten/lambda-color.bi](handwritten-output/lambda-color.png)

## Running your code on GHCi

See the readme for a1part1.

## How to test your implementation

See the readme for a1part1 for reading the output of `stack test`.

All graded tests (including some of the extra credit) are actual Bismuth
programs.  The remaining tests are there to make sure you are using a correct
implementation of functional images, and to make sure that you didn't alter the
cases given to you.  See the _Grading_ section for an explanation of the
relevant test suites.

You can run all of the aforementioned test programs using `stack run` to debug
your implementation.

### Property-based testing

See the readme for a1part1.  The graded tests in this assignments don't use
property-based testing.  The initial seed for this assignment is 83 (the atomic
number of Bismuth).  See the grading section for other arguments.

## Debugging your program

Haskell programs are lazy and pure, so they aren't as amenable to methods like
print debugging but you have several options:

- You can use GHCi as an interactive debugger: See [this
  page](https://downloads.haskell.org/~ghc/9.2.4/docs/html/users_guide/ghci.html#the-ghci-debugger)
  for how to do it.
- You can run your program on smaller inputs, and define helper functions to try
  those inputs.
- You can use the `trace` function from the `Debug.Trace` module to print
  something when a value is *needed*.  This may be an overkill for images
  because the rendering function calls the final image many times.

## Grading

There are lots of test suites in the template.  3 of them are relevant to grading:

- The `handwritten` suite consists of small handwritten programs that test at
  most a few features at once.  Each test in this suite is worth 3 points.
- The `example` suite consists of more complex handwritten programs that should run
  successfully. Each test in this suite is worth 5 points.
- The `extra-credit` suite consists of the juxtapose and replicate tests.  These
  tests are part of the extra credit grade (see below).

For all test programs are in the folders below. You can see which test belongs
to which test suite above by reading `HandwrittenSpec.hs`:

- Successful tests are in the `handwritten/` directory, and they produce the
  output images in `handwritten-output/`.
- Test programs that should give a runtime error are in the
  `handwritten-failure` directory.  These programs deliberately cause things
  like type errors at run time.

You can run the tests in each suite using

```
stack test '--ta=--seed=83 --match /SUITE_NAME'
```

Don't forget the initial slash, the `SUITE_NAME` should be one of `handwritten`,
`example`, or `extra-credit`.

- Maximum grade in this assignment is 100 points.
- Your grade is based on the total number of tests in the `handwritten` and
  `example` categories you pass. Each test is worth the same amount of points.
- If you just copy over the tests and add conditionals to match them, you will
  get 0 for the assignment.
- If `Eval.hs` in your submission does not compile with the original files from
  this repository and my solution for assignment 1 part 2, then you get 0 for
  the assignment.

### Extra credit

There are two extra credit portions of this assignment (worth 1% of total course
credit each, graded separately).  General rules:

- The extra credit is submitted at the same time as your normal assignment, if
  you submit the extra credit part late, you use up late days.
- I will grade the extra credit separately, later in the semester.

#### Implementing juxtapose and replicate

For this extra credit portion, you just need to implement these functions and
submit as usual.  I will test them also on a private test suite.

#### Designing new language features

For this extra credit portion:

- You need to submit all of your work related to extra credit in a directory
  named `extra-credit/` in your repository's root.  This directory should
  contain a PDF file for your report, and .bi files for the Bismuth programs.
- Write 1--2 non-trivial Bismuth programs (you can take inspiration from the
  programs in the `examples/` directory).
- Reflect on the programs you wrote, and see what additional language features
  would have made writing these programs easier, describe this language feature
  in detail, along with how it would make your programs easier.  I am looking
  for more than just adding more operators or library functions, rather
  something at the language level that cannot be "just another transformation"
  or "just another case for BinOp, ...".  Overall, this should be a ~1 page
  essay, submitted as a PDF.
# Programming Assignment 2 Bismuth interpreter
# WeChat: cstutorcs

# QQ: 749389476

# Email: tutorcs@163.com

# Computer Science Tutor

# Programming Help

# Assignment Project Exam Help
