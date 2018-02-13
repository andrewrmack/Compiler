# compiler - Andrew Mack
This is a being written for compiler for
[CSC-312](http://www.cs.grinnell.edu/~osera/courses/csc312/18sp/).

## Building
### Getting Stack
If you don't already have stack, you can obtain it by running
```
$ curl -sSL https://get.haskellstack.org/ | sh
```
More detailed instructions can be found in the
[Stack guide](https://docs.haskellstack.org). The project should also be
buildable with cabal, but you may need to offer a blood sacrifice to Haskell
Curry to ensure a successful build.

### Building the Project
To build the project, run
```
$ stack build
```
### Running the Project and Test Suite
To run the project, run
```
$ stack exec compiler
```

Arguments must passed after a `--` so that they are passed to `compiler` rather
than `stack`.
```
$ stack exec compiler -- foo bar baz
```

To run the test suite, run
```
$ stack test
```

## Changelog

### [assignment-03] - 2018-02-12
#### Added
- Somewhat accurate location information in errors
#### Changed
- The test suite is more robust
- The syntax now uses infix notation
    * The less than or equal to operator is non associative and binds the
        loosest
    * Addition and subtraction are left associative and bind tighter than less
        than or equal to
    * Multiplication and division are left associative and bind tighter than
        addition and subtraction
#### Known Bugs
- still none

### [assignment-02] - 2018-02-06
#### Added
- A lexer
- A parser
- An interpreter
#### Changed
- It now compiles things instead of spitting back text
#### Known Bugs
- still none

### [assignment-01] - 2018-01-29
#### Added
- Created the project
- Parse command line options
- Test suite
#### Changed
- N/A
#### Known Bugs
- none
