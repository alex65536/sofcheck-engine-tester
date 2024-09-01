# Utilities to test SoF Check chess engine

[![Build Status][build-badge]][build-url]

[build-badge]: https://github.com/alex65536/sofcheck-engine-tester/actions/workflows/build.yml/badge.svg?branch=master
[build-url]: https://github.com/alex65536/sofcheck-engine-tester/actions/workflows/build.yml

These utilities include:

- [Battlefield](#battlefield) (deprecated, development continues [3][here])
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

**NOTE:** Pascal/Lazarus version of Battlefield is deprecated and will no longer receive updates.
The utility has been rewritten in Go and is now located [3][here]. To get the latest version, you
can use the following command:

~~~~~
$ go install github.com/alex65536/day20/cmd/bfield@latest
~~~~~

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
[3]: https://github.com/alex65536/day20
