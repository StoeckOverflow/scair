# convolution_benchmark

$$
O_h = H-K_h+1,\qquad O_w = W-K_w+1
$$

$$
Y_{n,c_o,o_h,o_w}
=
\sum_{c_i=0}^{C_i-1}
\sum_{k_h=0}^{K_h-1}
\sum_{k_w=0}^{K_w-1}
X_{n,c_i,o_h+k_h,o_w+k_w}\,
K_{c_o,c_i,k_h,k_w}
$$

$$
\mathrm{checksum}
=
\sum_{n=0}^{N-1}
\sum_{c_o=0}^{C_o-1}
\sum_{o_h=0}^{O_h-1}
\sum_{o_w=0}^{O_w-1}
Y_{n,c_o,o_h,o_w}
$$

Tests:
- core Conv2D runtime and lowering evidence over representative selected sizes
- compares upstream MLIR, ScaIR dynamic, and value-dependent routes

Use in thesis:
- supports selected-kernel execution and overhead claims
- large image-like cases may use fewer driver iterations per timing sample to keep final runs executable
- not cache-tuned factorization evidence and not a broad Conv2D optimization claim
