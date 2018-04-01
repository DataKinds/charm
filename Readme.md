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
p := dup pp
print := p newline
flip := 0 1 swap
stackNTimes := [ 1 - dup ] [ flip dup 0 2 swap stackNTimes ] [ ] ifthen pop
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

## [DOCUMENTATION](https://github.com/aearnus/charm/blob/master/Documentation.md)

## TODO:

- write a prelude
- more documentation of functions
