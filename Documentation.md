# DOCUMENTATION

Charm has an extremely simple syntax. Everything is space delimited, and there is only one special construct - the function definition. Functions are defined using `function name := function body`. Lists are defined using `[ ]`. Strings are defined using `" "`.

Many functions are preprogrammed (in C++) into Charm. This includes object and stack manipulation, arithmetic, and some combinators. But, others are in the standard library of Charm, called `Prelude.charm`. The syntax for the documentation is as follows (taking `swap`, as an example):

function name - function description - function arguments

- `swap` - swaps two values around on the stack -

All arguments are popped off the stack, and explained in the order that you should pass them (reverse order from how they actually are on the stack).

## PREPROGRAMMED FUNCTIONS

### IO

- print:
