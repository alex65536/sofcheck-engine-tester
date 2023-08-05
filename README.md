# Utilities to test SoF Check chess engine

[![Build Status][build-badge]][build-url]

[build-badge]: https://github.com/alex65536/sofcheck-engine-tester/actions/workflows/build.yml/badge.svg?branch=master
[build-url]: https://github.com/alex65536/sofcheck-engine-tester/actions/workflows/build.yml

These utilities include:

- [Battlefield](#battlefield)
- [Data generator for selftests](#data-generator-for-selftests)
- [Test position set for BattleField](#test-position-set-for-battlefield)
- [SoFGameSet utilities](#sofgameset-utilities)

This project is mostly written in Pascal, as it relies on core libraries of
[Chess 256](https://github.com/alex65536/Chess256). These libraries were taken from the source
code of Chess 256 and slightly modified.

## Dependencies

On Debian/Ubuntu, you need to install the following packages:

- `fpc (>= 3.0.4)`
- `lcl-units (>= 2.0.0)`
- `lcl-utils (>= 2.0.0)`
- `python3 (>= 3.7)`

## Utilities

### Battlefield

BattleField is a tool to run many micro engine matches. It allows two engines to play many games
either on fixed time per move or on fixed depth.

To build this utility, run

~~~~~
$ cd battlefield
$ lazbuild Battlefield.lpi
~~~~~

Alternatively, you can open and build the project `battlefield/Battlefield.lpi` in Lazarus IDE.

The resulting binary is located in `battlefield/bin/battlefield`. Run the binary without arguments
to get more information on how to use it.

### Data generator for selftests

This small tool generates the positions for
[self-tests][1]. To compile and run it, use

~~~~~
$ cd selftest_data_create
$ lazbuild SelftestDataCreate.lpi
$ cd bin
$ ./selftest_data_create >boards.fen
~~~~~

The resulting `boards.fen` will be the same as `boards.fen` which is used for self-tests in
SoFCheck repository.

### Test position set for BattleField

This is not a utility, but a set of positions on which the engine evaluation function is tested.
These positions can be used with BattleField.

### SoFGameSet utilities

This is a set of utilities to work with [SoFGameSet][2] format. These utilities are written in
Python.

# License

GNU GPL v3 (or any later version)

[1]: https://github.com/alex65536/sofcheck/tree/master/selftest
[2]: https://github.com/alex65536/sofcheck/blob/master/docs/gameset.md
