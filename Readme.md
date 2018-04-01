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

## [OLD, UNUPDATED DOCUMENTATION](https://github.com/aearnus/charm/blob/master/docs/Documentation.md)

## [NEW DOCUMENTATION](dat://f6365c0b3fb82a732d125dc091b2dfc3518f39bbe8f0acdbf8956128ddd6b078/glossary.html)

Use the [Beaker Browser](https://beakerbrowser.com/) to access this peer to peer link:

dat://f6365c0b3fb82a732d125dc091b2dfc3518f39bbe8f0acdbf8956128ddd6b078/glossary.html

(Please keep your browser open and this site!)

Or, use [this link](http://68.3.184.237:4567/glossary.html). Note: this is run on my own server and not guaranteed to always be up.

Or, use [this file](https://github.com/Aearnus/charm/blob/master/docs/glossary.html).

## TODO:

- write a prelude
- more documentation of functions
