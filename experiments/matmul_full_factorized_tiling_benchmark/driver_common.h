#ifndef MATMUL_FULL_FACTORIZED_DRIVER_COMMON_H
#define MATMUL_FULL_FACTORIZED_DRIVER_COMMON_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "matmul_full_factorized_tiling"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "unknown"
#endif

enum {
#ifndef MATMUL_FULL_FACTORIZED_M0
#define MATMUL_FULL_FACTORIZED_M0 2
#endif
#ifndef MATMUL_FULL_FACTORIZED_M1
#define MATMUL_FULL_FACTORIZED_M1 64
#endif
#ifndef MATMUL_FULL_FACTORIZED_N0
#define MATMUL_FULL_FACTORIZED_N0 2
#endif
#ifndef MATMUL_FULL_FACTORIZED_N1
#define MATMUL_FULL_FACTORIZED_N1 64
#endif
#ifndef MATMUL_FULL_FACTORIZED_K0
#define MATMUL_FULL_FACTORIZED_K0 12
#endif
#ifndef MATMUL_FULL_FACTORIZED_K1
#define MATMUL_FULL_FACTORIZED_K1 64
#endif
  kM0 = MATMUL_FULL_FACTORIZED_M0,
  kM1 = MATMUL_FULL_FACTORIZED_M1,
  kN0 = MATMUL_FULL_FACTORIZED_N0,
  kN1 = MATMUL_FULL_FACTORIZED_N1,
  kK0 = MATMUL_FULL_FACTORIZED_K0,
  kK1 = MATMUL_FULL_FACTORIZED_K1,
  kK = kK0 * kK1,
  kM = kM0 * kM1,
  kN = kN0 * kN1,
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
    B[i] = (float)(((i * 3) % 11) + 1) / 16.0f;
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

#endif
