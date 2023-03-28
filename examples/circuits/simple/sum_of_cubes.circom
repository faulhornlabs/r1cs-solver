pragma circom 2.0.0;

// computes the sum of cubes of an array

template SumOfCubes(n) {
  signal input  inp[n];
  signal output out;

  signal aux[n];
  signal aux2[n];

  var sum = 0;
  for(var i=0; i<n; i++) {
    aux[i]  <== inp[i] * inp[i];
    aux2[i] <== aux[i] * inp[i];
    sum += aux2[i];
  }

  out <== sum;
}