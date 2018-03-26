# DOCUMENTATION

Charm has an extremely simple syntax. Everything is space delimited, and there is only one special construct - the function definition. Functions are defined using `function name := function body`. Lists are defined using `[ ]`. Strings are defined using `" "`. Numbers can be either integers (`long long`s) or floats (`long double`s). The stack is initialized to 20000 (to be changed) zero integers.

Everything in Charm is a function - there are number functions, string functions, and list functions.

Many functions are preprogrammed (in C++) into Charm. This includes object and stack manipulation, arithmetic, and some combinators. But, others are in the standard library of Charm, called `Prelude.charm`. The syntax for the documentation is as follows (taking `swap`, as an example):

function name - function description - function arguments

- `swap` - swaps two functions - s[1]: the first function's pointer, s[2]: the second function's pointer

All arguments are popped off the stack, and explained in the order that you should pass them (reverse order from how they actually are on the stack).

## PREPROGRAMMED FUNCTIONS

### IO

- `pp` - pops a value and prints it - s[0]: function to print

- `newline` - prints a newline

### STACK MANIPULATION

- `dup` - duplicates a function - s[0]: function to duplicate

- `pop` - pops a function - s[0]: function to pop

- `swap` - swaps two functions - s[1]: the first function's pointer integer, s[0]: the second function's pointer integer

### LIST / STRING MANIPULATION

- `len` - returns length of function - s[0]: list or string to measure length of

- `at` - returns element at index of function - s[1]: list or string to index, s[0]: index integer

- `insert` - inserts element at index of function - s[2]: function to be inserted in, s[1]: function to insert, s[0]: index to insert integer

- `concat` - concatenates two lists or strings - s[1]: first function to concatenate, s[0]: second function to concatenate

### ARITHMETIC (NOTE: floating point operations aren't implemented yet)

- `+` - adds - s[1], s[0]

- `-` - subtracts - s[1], s[0]

- `*` - multiplies - s[1], s[0]