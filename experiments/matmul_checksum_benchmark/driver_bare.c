#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "matmul_checksum"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "unknown"
#endif

extern void matmul_dynamic(
    int64_t n_nat, int64_t m_nat, int64_t k_nat,
    float *A,
    float *B,
    float *C);

extern void checksum_dynamic(
    int64_t n_nat, int64_t m_nat,
    float *C,
    float *out);

static double elapsed_ns(struct timespec start, struct timespec end) {
  return (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
         (double)(end.tv_nsec - start.tv_nsec);
}

int main(int argc, char **argv) {
  int64_t iterations = 10;
  if (argc < 4 || argc > 5) {
    fprintf(stderr, "usage: %s n m k [iterations]\n", argv[0]);
    return 1;
  }

  const int64_t n = strtoll(argv[1], NULL, 10);
  const int64_t m = strtoll(argv[2], NULL, 10);
  const int64_t k = strtoll(argv[3], NULL, 10);
  if (argc > 4) iterations = strtoll(argv[4], NULL, 10);
  if (iterations <= 0) {
    fprintf(stderr, "iterations must be positive\n");
    return 1;
  }

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

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    for (int64_t i = 0; i < n * m; ++i) C[i] = 0.0f;
    out[0] = 0.0f;
    matmul_dynamic(n, m, k, A, B, C);
    checksum_dynamic(n, m, C, out);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  float expected = (float)(n * m * k);
  if (out[0] != expected) {
    fprintf(stderr, "unexpected checksum: got %.1f expected %.1f\n", out[0], expected);
    free(A);
    free(B);
    free(C);
    free(out);
    return 3;
  }

  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  printf("result=%.1f\n", out[0]);
  printf("expected_result=%.1f\n", expected);
  printf("iterations=%lld\n", (long long)iterations);
  printf("total_ns=%.0f\n", elapsed_ns(start, end));
  printf("ns_per_iter=%.2f\n", elapsed_ns(start, end) / (double)iterations);

  free(A);
  free(B);
  free(C);
  free(out);
  return 0;
}
