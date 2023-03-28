
Example circuits and their soundness tests
------------------------------------------

Here we show how to test actual circuit components. The basic idea
is to define the expected semantics using a very high-level description, and 
then the rest is pretty much automatic (at least when it works, and is fast enough).

For now there are three categories of example tests:

- a few simple example circuits
- the 2023 Berkeley ZKP MOOC Lab exercises
- some of the simpler circuits from `circomlib`

### 2023 Berkeley ZKP MOOC Lab exercises

We test the components of the lab exercise, which is an 
implementation of a simplified floating point addition.

**NOTE:** The organizers asked not to publish the actual solutions, so we only
include the tests and the unsolved solution template here. You have to solve
the exercises yourself.

See <https://github.com/rdi-berkeley/zkp-mooc-lab/> for the details.

Unfortunately testing the final `FloatAdd` template is _very_ slow, because
for some branches of the code (for example: adding zeros, or too big difference
in the exponents) the search space blows up even with a very small number of
exponent / mantissa bits.


