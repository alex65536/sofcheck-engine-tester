# What is PyField?

PyField is an attempt to rewrite the [Battlefield](../README.md#battlefield) utility into Python.

# Running PyField

First, install the dependencies:

```
$ pip install chess colorama
```

Then, you can just run the utility as

```
$ python3 pyfield.py <args>
```

The command-line arguments are compatible with those of Battlefield. Note that PyField is very
experimental now, so some command-line arguments of Battlefield may be unsupported

# Pros and cons over Battlefield

## Pros

- Easier to maintain, less code
- Easier to contribute (more people who know Python than the ones who know Pascal)
- Easier to find the libraries if we need to integrate some advanced features

## Cons

- Battlefield is a single binary without any dependencies, but PyField required python plus some
  external dependencies
- PyField is slow. Its speed is comparable to Battlefield in single-threaded tests, but it is
  reduced dramatically when running multithreaded. This is because of GIL and overall speed of
  Python (by the way, PyPy doesn't help here). We can overcome the GIL by some code rewrite, as the
  threads don't require much communication. The second issue is harder to deal, especially
  considering that we can boost the chess rules in Battlefield and cannot do it in PyField
