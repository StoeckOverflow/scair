#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "matmul_tiling"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "mlir_baseline"
#endif

enum {
#ifndef MATMUL_TILING_M
#define MATMUL_TILING_M 128
#endif
#ifndef MATMUL_TILING_N
#define MATMUL_TILING_N 128
#endif
#ifndef MATMUL_TILING_K0
#define MATMUL_TILING_K0 12
#endif
#ifndef MATMUL_TILING_K1
#define MATMUL_TILING_K1 64
#endif
  kM = MATMUL_TILING_M,
  kN = MATMUL_TILING_N,
  kK0 = MATMUL_TILING_K0,
  kK1 = MATMUL_TILING_K1,
  kK = kK0 * kK1,
  kAElements = kM * kK,
  kBElements = kK * kN,
  kCElements = kM * kN,
};

typedef struct {
  float *allocated;
  float *aligned;
  int64_t offset;
  int64_t sizes[1];
  int64_t strides[1];
} MemRef1D_f32;

extern void _mlir_ciface_matmul_tiling(
    int64_t m, int64_t n, int64_t k0, int64_t k1,
    MemRef1D_f32 *A, MemRef1D_f32 *B, MemRef1D_f32 *C);

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

static void init_inputs(float *A, float *B) {
  for (int64_t i = 0; i < kAElements; ++i) {
    A[i] = (float)((i % 7) + 1) / 8.0f;
  }
  for (int64_t i = 0; i < kBElements; ++i) {
    B[i] = (float)(((i * 3) % 11) - 5) / 16.0f;
  }
}

static void reference_matmul(const float *A, const float *B, float *C) {
  for (int64_t i = 0; i < kM; ++i) {
    for (int64_t j = 0; j < kN; ++j) {
      float sum = 0.0f;
      for (int64_t p = 0; p < kK; ++p) {
        sum += A[i * kK + p] * B[p * kN + j];
      }
      C[i * kN + j] = sum;
    }
  }
}

static float checksum(const float *ptr, int64_t n) {
  float sum = 0.0f;
  for (int64_t i = 0; i < n; ++i) sum += ptr[i];
  return sum;
}

static int verify_close(const float *got, const float *expected, int64_t n) {
  for (int64_t i = 0; i < n; ++i) {
    float diff = got[i] - expected[i];
    if (diff < 0.0f) diff = -diff;
    if (diff > 1e-3f) return 0;
  }
  return 1;
}

int main(int argc, char **argv) {
  int64_t iterations = 100;
  if (argc > 1) iterations = strtoll(argv[1], NULL, 10);
  if (iterations <= 0) {
    fprintf(stderr, "iterations must be positive\n");
    return 1;
  }

  float *A = (float *)malloc((size_t)kAElements * sizeof(float));
  float *B = (float *)malloc((size_t)kBElements * sizeof(float));
  float *C = (float *)malloc((size_t)kCElements * sizeof(float));
  float *Ref = (float *)malloc((size_t)kCElements * sizeof(float));
  if (!A || !B || !C || !Ref) {
    fprintf(stderr, "allocation failed\n");
    free(A);
    free(B);
    free(C);
    free(Ref);
    return 2;
  }

  init_inputs(A, B);
  fill(C, kCElements, 0.0f);
  fill(Ref, kCElements, 0.0f);
  reference_matmul(A, B, Ref);
  const float expected = checksum(Ref, kCElements);

  MemRef1D_f32 Aref = make1D(A, kAElements);
  MemRef1D_f32 Bref = make1D(B, kBElements);
  MemRef1D_f32 Cref = make1D(C, kCElements);

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    fill(C, kCElements, 0.0f);
    _mlir_ciface_matmul_tiling(kM, kN, kK0, kK1, &Aref, &Bref, &Cref);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  const float result = checksum(C, kCElements);
  if (!verify_close(C, Ref, kCElements)) {
    fprintf(stderr, "unexpected matmul result\n");
    printf("run_status=fail\n");
    printf("result=%.9g\n", result);
    printf("expected_result=%.9g\n", expected);
    printf("ns_per_iter=%.2f\n", elapsed_ns(start, end) / (double)iterations);
    free(A);
    free(B);
    free(C);
    free(Ref);
    return 3;
  }

  printf("run_status=ok\n");
  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  printf("result=%.9g\n", result);
  printf("expected_result=%.9g\n", expected);
  printf("checksum=%.9g\n", result);
  printf("iterations=%lld\n", (long long)iterations);
  printf("total_ns=%.0f\n", elapsed_ns(start, end));
  printf("ns_per_iter=%.2f\n", elapsed_ns(start, end) / (double)iterations);

  free(A);
  free(B);
  free(C);
  free(Ref);
  return 0;
}
