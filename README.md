# compiler - Andrew Mack
This is a being written for compiler for
[CSC-312](http://www.cs.grinnell.edu/~osera/courses/csc312/18sp/).
Currently, it doesn't actually compile anything.

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

### [assignment-01] - 2018-01-29
#### Added
- Created the project
- Parse command line options
- Test suite
#### Changed
- N/A
#### Known Bugs
- none
