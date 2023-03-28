pragma circom 2.0.0;

// this is an example of an unsound circuit.
//
// the goal is to right-shift the input by `k` bits 
//
// this circuit would work properly over the integers; 
// however in a finite field, you can divide _any_ field 
// element by two, not just even numbers. Hence, there
// will be 2^k witness solutions. But if you only test 
// completeness, this problem will not show up.

template RightShiftUnsound(k) {
  signal input  inp;
  signal output out;  

  signal aux[k+1];
  signal bit[k];

  aux[0] <== inp;
  for(var i=0; i<k; i++) {
    aux[i+1] <-- aux[i] >> 1;           // shift right by 1
    bit[i] <== aux[i] - 2*aux[i+1];     // compute the remainder
    bit[i] * (1-bit[i]) === 0;          // ensure that the remainder is either 0 or 1
  }

  aux[k] ==> out;
}