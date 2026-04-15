#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "strided_matmul"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "mlir_baseline"
#endif

typedef struct {
  float *allocated;
  float *aligned;
  int64_t offset;
  int64_t sizes[1];
  int64_t strides[1];
} MemRef1D_f32;

extern void _mlir_ciface_matmul_strided(
    int64_t n, int64_t m, int64_t k,
    int64_t a_stride0, int64_t a_stride1,
    int64_t b_stride0, int64_t b_stride1,
    int64_t c_stride0, int64_t c_stride1,
    MemRef1D_f32 *Aflat, MemRef1D_f32 *Bflat, MemRef1D_f32 *Cflat);

static double elapsed_ns(struct timespec start, struct timespec end) {
  return (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
         (double)(end.tv_nsec - start.tv_nsec);
}

static MemRef1D_f32 make1D(float *ptr, int64_t n) {
  MemRef1D_f32 m;
  m.allocated = ptr;
  m.aligned = ptr;
  m.offset = 0;
  m.sizes[0] = n;
  m.strides[0] = 1;
  return m;
}

static void fill(float *ptr, int64_t n, float value) {
  for (int64_t i = 0; i < n; ++i) ptr[i] = value;
}

static int verify(float *C, int64_t n, int64_t m, int64_t k, int64_t stride0) {
  const float expected = (float)k;
  for (int64_t i = 0; i < n; ++i) {
    for (int64_t j = 0; j < m; ++j) {
      if (C[i * stride0 + j] != expected) return 0;
    }
  }
  return 1;
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
  if (n <= 0 || m <= 0 || k <= 0 || iterations <= 0) {
    fprintf(stderr, "n, m, k, and iterations must be positive\n");
    return 1;
  }

  const int64_t a_stride0 = k + 3;
  const int64_t a_stride1 = 1;
  const int64_t b_stride0 = m + 5;
  const int64_t b_stride1 = 1;
  const int64_t c_stride0 = m + 7;
  const int64_t c_stride1 = 1;
  const int64_t a_total = n * a_stride0;
  const int64_t b_total = k * b_stride0;
  const int64_t c_total = n * c_stride0;

  float *A = (float *)malloc((size_t)a_total * sizeof(float));
  float *B = (float *)malloc((size_t)b_total * sizeof(float));
  float *C = (float *)malloc((size_t)c_total * sizeof(float));
  if (!A || !B || !C) {
    fprintf(stderr, "allocation failed\n");
    free(A);
    free(B);
    free(C);
    return 2;
  }

  fill(A, a_total, 1.0f);
  fill(B, b_total, 1.0f);

  MemRef1D_f32 Aref = make1D(A, a_total);
  MemRef1D_f32 Bref = make1D(B, b_total);
  MemRef1D_f32 Cref = make1D(C, c_total);

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    fill(C, c_total, 0.0f);
    _mlir_ciface_matmul_strided(
        n, m, k,
        a_stride0, a_stride1,
        b_stride0, b_stride1,
        c_stride0, c_stride1,
        &Aref, &Bref, &Cref);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  if (!verify(C, n, m, k, c_stride0)) {
    fprintf(stderr, "unexpected matmul result\n");
    free(A);
    free(B);
    free(C);
    return 3;
  }

  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  printf("result=%.1f\n", C[(n - 1) * c_stride0 + (m - 1)]);
  printf("expected_result=%.1f\n", (float)k);
  printf("iterations=%lld\n", (long long)iterations);
  printf("total_ns=%.0f\n", elapsed_ns(start, end));
  printf("ns_per_iter=%.2f\n", elapsed_ns(start, end) / (double)iterations);

  free(A);
  free(B);
  free(C);
  return 0;
}
