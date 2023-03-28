pragma circom 2.0.0;

// returns 1 if the input is in the set {0,1}, and zero otherwise

template IsBoolean() {
    signal input in;
    signal output out;
    signal aux,inv;

    aux <== in * (in-1);              // 0 if and only if `in={0,1}`
    inv <-- (aux == 0 ? 0 : 1/aux);   // otherwise it has an inverse
    out <== 1 - aux*inv;              // if `aux` is 0, then `out` must be 1?
    out * aux === 0;                  // if `aux `is not 0, then `out` must be 0.
}
