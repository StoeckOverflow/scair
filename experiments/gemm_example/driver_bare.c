#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern void matmul_dynamic(
    int64_t n, int64_t m, int64_t k,
    int64_t A_s0, int64_t A_s1, float *A,
    int64_t B_s0, int64_t B_s1, float *B,
    int64_t C_s0, int64_t C_s1, float *C);

extern void checksum_dynamic(
    int64_t n, int64_t m,
    int64_t C_s0, int64_t C_s1, float *C,
    int64_t out_s0, float *out);

int main(int argc, char **argv) {
  if (argc != 4) {
    fprintf(stderr, "usage: %s n m k\n", argv[0]);
    return 1;
  }

  const int64_t n = strtoll(argv[1], NULL, 10);
  const int64_t m = strtoll(argv[2], NULL, 10);
  const int64_t k = strtoll(argv[3], NULL, 10);

  float *A = (float *)malloc((size_t)(n * k) * sizeof(float));
  float *B = (float *)malloc((size_t)(k * m) * sizeof(float));
  float *C = (float *)malloc((size_t)(n * m) * sizeof(float));
  float *out = (float *)malloc(sizeof(float));

  if (!A || !B || !C || !out) {
    fprintf(stderr, "allocation failed\n");
    free(A);
    free(B);
    free(C);
    free(out);
    return 2;
  }

  for (int64_t i = 0; i < n * k; ++i) A[i] = 1.0f;
  for (int64_t i = 0; i < k * m; ++i) B[i] = 1.0f;
  for (int64_t i = 0; i < n * m; ++i) C[i] = 0.0f;
  out[0] = 0.0f;

  matmul_dynamic(n, m, k, k, 1, A, m, 1, B, m, 1, C);
  checksum_dynamic(n, m, m, 1, C, 1, out);

  printf("checksum = %.1f\n", out[0]);

  free(A);
  free(B);
  free(C);
  free(out);
  return 0;
}
