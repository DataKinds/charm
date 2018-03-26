# Charm - a stack based functional language

This is based off of Forth and Joy.

`" hello " " world " concat p                      => hello world`
`[ 1 2 ] [ 3 4 ] i 0 2 swap i pp pp pp pp newline  => 2134`

## DEPENDENCIES:

- libreadline and related development packages (`libreadline-dev on apt-based systems`)

## COMPILATION:

```
make
./charm
```

To build with debug mode enabled (warning: very verbose!), use `make DEBUG=true`.

## [DOCUMENTATION](https://github.com/aearnus/charm/blob/master/Documentation.md)

## TODO:

- write a prelude
- more documentation of functions
