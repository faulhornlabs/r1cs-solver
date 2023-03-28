pragma circom 2.0.0;

// computes a vector, whose `idx`-th 
// coordinate is 1 and the rest is 0
// it will fail if `0 <= idx < n` is not satisfied

template StdBasisVector(n) {
    signal input  idx;
    signal output vec[n];

    // compute and check a vector ind consisting of {0,1} such
    // that it starts with 1, ends with 0 and is constant 
    // apart from the jump at `idx`

    signal ind[n+1];
    for(var i=0; i <= n; i++) {
      ind[i] <-- (i<=idx) ? 1 : 0;
      if (i>0) {
        (ind[i] - ind[i-1]) * (idx - i + 1) === 0;
      }
    }
    ind[0] === 1;
    ind[n] === 0;

    for(var i=0; i < n; i++) {
      vec[i] <== ind[i] - ind[i+1];
    }
}