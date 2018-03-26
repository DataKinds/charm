# DOCUMENTATION

Charm has an extremely simple syntax. Everything is space delimited, and there is only one special construct - the function definition. Functions are defined using `function name := function body`. Lists are defined using `[ ]`. Strings are defined using `" "`. Numbers can be either integers (`long long`s) or floats (`long double`s). The stack is initialized to 20000 (to be changed) zero integers.

Everything in Charm is a function - there are number functions, string functions, and list functions.

Many functions are preprogrammed (in C++) into Charm. This includes object and stack manipulation, arithmetic, and some combinators. But, others are in the standard library of Charm, called `Prelude.charm`. The syntax for the documentation is as follows (taking `swap`, as an example):

function name - function description - function arguments

- `swap` - swaps two functions - s[1]: the first function's pointer, s[2]: the second function's pointer

All arguments are popped off the stack, and explained in the order that you should pass them (reverse order from how they actually are on the stack).

## PREPROGRAMMED FUNCTIONS

### CONTROL FLOW

- `ifthen` - takes a condition to run, then a list to run if the top of the stack is greater than 0 and a list to run if the top of the stack is less than 0 - s[2]: condition (as a list of functions), s[1]: truthy list (as a list of functions), s[2]: falsy list (as a list of functions).

NOTE: `ifthen` doesn't pop the condition afterwards.

- `i` - like Lisp's `unquote`, pop a list off the stack then run it - s[0]: the list to `i`nterpret

### IO

- `pp` - pops a value and prints it - s[0]: function to print

- `newline` - prints a newline

### STACK MANIPULATION

- `dup` - duplicates a function - s[0]: function to duplicate

- `pop` - pops a function - s[0]: function to pop

- `swap` - swaps two functions n elements from the top of the stack - s[1]: the first function's pointer integer, s[0]: the second function's pointer integer

### LIST / STRING MANIPULATION

- `len` - returns length of function - s[0]: list or string to measure length of

- `at` - returns element at index of function - s[1]: list or string to index, s[0]: index integer

- `insert` - inserts element at index of function - s[2]: function to be inserted in, s[1]: function to insert, s[0]: index to insert integer

- `concat` - concatenates two lists or strings - s[1]: first function to concatenate, s[0]: second function to concatenate

### ARITHMETIC (NOTE: floating point operations aren't implemented yet)

- `+` - adds - s[1], s[0]

- `-` - subtracts - s[1], s[0]

- `*` - multiplies - s[1], s[0]

### STACK CREATION / DESTRUCTION

NOTE: The default stack is christened the name `0`. Any stack that tries to create a new stack of name `0` will face _undefined behavior_.

- `createstack` - creates a new stack with a specified name and length - s[1]: the length of the stack, s[0]: the name of the stack (any type here is permissible!)

- `switchstack` - switch to the specified stack - s[0]: the name of the stack to switch to

### REFERENCE GETTING / SETTING

- `getref` - gets a reference to an object by the name specified, puts it on the top of the stack - s[0]: the name of the object

- `setref` - sets the value of an object referred to by the name specified - s[1]: the name of the object

### RIPPED OUT OF `PredefinedFunctions.CPP`

```
const std::vector<std::string> PredefinedFunctions::cppFunctionNames = {
	//INPUT / OUTPUT
	"pp", "newline",
	//STACK MANIPULATIONS
	"dup", "pop", "swap",
	//TRAVERSABLE (STRING/LIST) MANIPULATIONS
	"len", "at", "insert", "concat",
	//LIST MANIPULATIONS
	//TODO
	"fold", "map", "zip",
	//STRING MANIPULATIONS
	//TODO
	"tocharlist", "fromcharlist"
	//CONTROL FLOW
	"i", "ifthen",
	//BOOLEAN OPS - TRUE: >=1, FALSE: <1 - INTEGER ONLY
	"nor",
	//TYPE INSPECIFIC MATH
	"abs",
	//INTEGER OPS
	"+", "-", "/", "*", "%", "toint",
	//FLOAT OPS
	"+f", "-f", "/f", "*f", "tofloat",
	//STACK CREATION/DESTRUCTION
	//TODO
	"createstack", "switchstack",
	//REF GETTING/SETTING
	//TODO
	"getref", "setref"
};
```

## PRELUDE FUNCTIONS

```
p := dup pp
print := p newline
flip := 0 1 swap
stackNTimes := [ 1 - dup ] [ flip dup 0 2 swap stackNTimes ] [ ] ifthen pop
```
