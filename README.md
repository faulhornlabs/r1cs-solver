
R1CS solver
===========

`r1cs-solver`, copyright 2023 Faulhorn Labs.

What is this?
-------------

This is a small testing tool to help checking soundness of R1CS circuits written 
using [`circom`](https://docs.circom.io), or possibly other similar tools.

What it does is to use a heuristic algorithm to try and solve the R1CS equations,
typically with some inputs fixed. The idea is that a typical piece of `circom` code 
(called "template" in `circom` jargon) usually implements some normal program 
functionality (ie. a regular function) for which we have a semantic model. 
If for all possible inputs (or at least a well selected subset of them) the 
equations have a unique solution, and the output matches the expected semantics, 
then we can be reasonably sure that our code is _sound_
(we haven't make a mistake of forgetting important constraints, and a malicious
prover cannot abuse the given piece of code).

Getting started
---------------

This is a [Haskell](https://www.haskell.org/) library, which you can import and 
use in your testing code. Not surprisingly you will need 
[the Haskell toolchain](https://www.haskell.org/get-started/), and some familiarity
with the Haskell language (you have to write your expected semantics in Haskell);
and you will also need [`circom`](https://docs.circom.io/getting-started/installation/) 
and `snarkjs` (and the latter requires `nodejs`). 
After that you can use the standard `cabal` workflow. 

For now there are a lot of examples in the `examples` subdirectory. More detailed
description will come later.

Note: The software is untested on Windows (there could be minor issues with executing 
external programs and manipulating paths). It should be fine on Unix-like systems
(include MacOS).

What is R1CS?
-------------

R1CS is short for "rank-1 constraint systems", which means set of quadratic equations
of the form `A*B - C = 0` where `A`, `B` and `C` are (affine) linear expressions
involving a set of so called "witness variables", which are variables taking 
values in a fixed (large) finite field.

R1CS is a popular "backend format" for zero-knowledge proofs. `circom` is a
kind of macro-assembler language for writing R1CS constraints (or as often called,
circuits).

How does the equation solving work?
-----------------------------------

We iteratively look for the following special kinds of equations:

- a linear term equals zero
- the product of two linear terms equals zero
- only a single variable is present 
- trivial equation
- trivially contradictory equation

From these we can either eliminate a variable, eliminate an equation, or stop
solving because there is no solution.

If we can solve all possible branches, then we have a complete solution. The
conceptual idea behind this tool is that for "normal", human-written circom code 
with _fixed inputs_, this will be usually the case. Then we can check if the
output matches the desired semantics in _all solutions_.

This can provide a much higher confidence in the soundness of the code than some
other testing methods, because we are actually testing the _whole witness space_,
by reducing it to a managable set using algebraic manipulation.


