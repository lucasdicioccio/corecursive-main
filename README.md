# corecursive-main

On Hackage: https://hackage.haskell.org/package/corecursive-main .

Functional programmers are used to program with (co-)recursive functions.  What
if the main program was itself recursive? One could benefit the underlying
operating system scheduler to orchestrate resources. For instance, limiting
memory, file-descriptor on a per-recursive-call basis. Another instance is
supporting distributed computations across multiple machines.

## This package

This package demonstrates the idea of replacing the main entry-point of the
program (i.e., `main :: IO ()`) with a recursion-aware main. The program first
pattern matches on an "argument". This argument defines which action to take
for the rest of the program. The rest of the program runs with a
"Self-reference", which allows the program to call itself back. Explicitly
passing a Self-reference is very similar to how the `fix` function allows to
factor recursion out of the body of a self-referential function.

Unlike `fix`, which executes recursive-call in the same memory-space as the
call-site, developper using this package need to teach the program to transfer
arguments and return values back-and-forth the call-site and the execution
site. This package minimizes the developper effort to perform this translation.

Currently, one mode is supported: spawning a child process on the same OS while
serializing arguments over the command-line parameters. A library user with
many entry points may want to leverage the distributed-closure capability to
sidestep writing many encoder/decoder.

Future versions of this package or sibling packages will likely provide:
- support for 'JSON' or 'Dhall' serialization format
- support for 'stdin' or networked serialization mechanisms

## Envisionned usages

Distributed computating:
- run a same copy of the program on many places, each on a subset of the data (much like Map/Reduce)

Dev/Ops:
- use a single binary to reconfigure the whole datacenter and services after a cataclysmic outage (see #History) as well

- orchestrate processes locally

## History

This package is factored out of code and findings made while writing
[DepTrack](https://github.com/lucasdicioccio/deptrack-project). If you like
this package you may find other interesting ideas in DepTrack.
