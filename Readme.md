# Charm - a stack based functional language

This is based off of Forth and Joy.

`" hello " " world " concat p                      => hello world`

`[ 1 2 ] [ 3 4 ] i 0 2 swap i pp pp pp pp newline  => 2134`

Example REPL session:

```
tyler@nasa:~/scripts/charm$ ./charm
Charm Interpreter v0.0.1
Made by @Aearnus
Looking for Prelude.charm...
Prelude.charm loaded.

Charm$ addOneToN := " n " getref 1 + " n " flip setref
Charm$ " n " 0 setref
Charm$ addOneToN
Charm$ " n " getref print
1
Charm$ [ addOneToN ] dup dup dup dup dup dup
Charm$ concat concat concat concat concat
Charm$ print
[ addOneToN addOneToN addOneToN addOneToN addOneToN addOneToN ]
Charm$ i
Charm$ " n " getref print
7
```

## DEPENDENCIES:

- libreadline and related development packages (`libreadline-dev on apt-based systems`)

## COMPILATION:

```
make release
./charm-release
```

To build with debug mode enabled (warning: very verbose!), use `make DEBUG=true`.

## ABOUT CHARM

### Basic syntax and implementation

Charm has an extremely simple syntax. Everything is space delimited, and there is only one special construct - the function definition. Functions are defined using `function name := function body` and are tail call optimized for most use cases. Lists are defined using `[ ]`. Strings are defined using `" "`. Numbers can be either integers (`long long`s) or floats (`long double`s). The stack is initialized to 20000 (to be changed) zero integers.

Everything in Charm is a function - there are number functions, string functions, and list functions.

Many functions are preprogrammed (in C++) into Charm. This includes object and stack manipulation, arithmetic, and some combinators. But, others are in the standard library of Charm, called `Prelude.charm`. The glossary explains functions with its arguments using calling order, placing the deepest value on the stack first. This mirrors how it would be written with Charm itself.

### About optimization

Charm uses a self-written optimizing interpreter. I'm very interested in the use cases and the effectiveness of the optimizations. The interpreter performs two optimizations: inlining and tail-call.

Inlining optimization is enabled by default through the compilation option `-DOPTIMIZE_INLINE=true`. Inlining optimization occurs if the interpreter detects that a function isn't recursive. If it isn't, the interpreter writes in the contents of the function wherever it is called, instead of writing the function itself (like a text macro). This removes 1 (or more, depending on how deep the inlining goes) layer of function redirection.

Tail-call optimization is necessary for this language, as there are no other ways to achieve a looping construct but recursion. There are a few cases which get tail-call optimized into a loop. These few cases are:

* `f := <code> f`
* `f := [ <cond> ] [ <code> f ] [ <code> ] ifthen`
* `f := [ <cond> ] [ <code> ] [ <code> f ] ifthen`
* `f := [ <cond> ] [ <code> f ] [ <code> f ] ifthen` (gets unrolled into a loop of the first form, ends up looking like `f := [ <cond> ] [ <code> ] [ <code> ] ifthen f`)

(If you can think of any other cases or a more general case, please open an issue!). These optimizations should allow for looping code that does not smash the calling stack and significant speedups. If there are any cases where these optimizations seem to be causing incorrect side effects, please create an issue or get into contact with me.

## [FULL FUNCTION GLOSSARY](dat://f6365c0b3fb82a732d125dc091b2dfc3518f39bbe8f0acdbf8956128ddd6b078/glossary.html)

Use the [Beaker Browser](https://beakerbrowser.com/) to access this peer to peer link:

dat://f6365c0b3fb82a732d125dc091b2dfc3518f39bbe8f0acdbf8956128ddd6b078/

OR

dat://charm-glossary-aearnus.hashbase.io/

(Please keep your browser open and support this site!)

Or, use [this link](https://charm-glossary-aearnus.hashbase.io/). Note: this is run on https://hashbase.io/ and I can't guarantee it's veracity.

Or, use [this file](https://github.com/Aearnus/charm/blob/master/docs/index.html).

## SUPPORT OR DONATE

### Todo list

- C FFI & linkage
- Imports from other files
- A proper interpreter, not just a REPL
- Possible LLVM codegen?

### Links

[Open an issue](https://github.com/Aearnus/charm/issues/new) or [DM me on Twitter](https://twitter.com/aearnus).
