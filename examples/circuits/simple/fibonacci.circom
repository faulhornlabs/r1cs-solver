pragma circom 2.0.0;

// computes the `n`-th element of the generalized Fibonacci sequence
// with the given first two elements

template Fibonacci(n) {

  signal input  in[2];
  signal output out;

  var m = (n < 2) ? 2 : n+1;
  signal fib[m];

  fib[0] <== in[0];
  fib[1] <== in[1];

  for(var i=2; i<m; i++) {
    fib[i] <== fib[i-2] + fib[i-1];
  }

  fib[n] ==> out;

}