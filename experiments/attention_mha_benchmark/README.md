# attention_mha_benchmark

$$
H = \mathrm{heads}\cdot D
$$

$$
S_{b,h,i,j}
= \frac{1}{\sqrt{D}}
  \sum_{d=0}^{D-1}
  Q_{b,i,hD+d}\,K_{b,j,hD+d}
$$

$$
P_{b,h,i,j}
=
\frac{\exp\!\left(S_{b,h,i,j}-\max_{0\leq t< L} S_{b,h,i,t}\right)}
     {\sum_{t=0}^{L-1}\exp\!\left(S_{b,h,i,t}-\max_{0\leq u< L} S_{b,h,i,u}\right)}
$$

$$
O_{b,i,hD+d}
=
\sum_{j=0}^{L-1}
P_{b,h,i,j}\,V_{b,j,hD+d}
$$

Tests:
- supporting attention/MHA route with explicit $H=\mathrm{heads}\cdot D$ factorization
- structural/code-generation evidence for preserving head and head-dimension information

Use in thesis:
- supporting evidence only; not a headline runtime benchmark
- does not isolate full model performance or general attention optimization
