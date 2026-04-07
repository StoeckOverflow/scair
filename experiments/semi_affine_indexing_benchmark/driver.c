#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "semi_affine_fill_and_sum"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "unknown"
#endif

typedef struct {
  float *allocated;
  float *aligned;
  int64_t offset;
  int64_t sizes[1];
  int64_t strides[1];
} MemRef1D_f32;

extern void _mlir_ciface_semi_affine_fill_and_sum(
    int64_t stride0, int64_t stride1, MemRef1D_f32 *flat, MemRef1D_f32 *out);

static MemRef1D_f32 make1D(float *ptr, int64_t n) {
  MemRef1D_f32 m;
  m.allocated = ptr;
  m.aligned = ptr;
  m.offset = 0;
  m.sizes[0] = n;
  m.strides[0] = 1;
  return m;
}

int main(int argc, char **argv) {
  const int64_t stride0 = 1024;
  const int64_t stride1 = 1;
  const int64_t total = 256 * stride0;
  int64_t iterations = 100;

  if (argc > 1) iterations = strtoll(argv[1], NULL, 10);
  if (iterations <= 0) {
    fprintf(stderr, "iterations must be positive\n");
    return 1;
  }

  float *flat = (float *)malloc((size_t)total * sizeof(float));
  float *out = (float *)malloc(sizeof(float));
  if (!flat || !out) {
    fprintf(stderr, "allocation failed\n");
    free(flat);
    free(out);
    return 2;
  }

  MemRef1D_f32 flat_ref = make1D(flat, total);
  MemRef1D_f32 out_ref = make1D(out, 1);

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    for (int64_t i = 0; i < total; ++i) flat[i] = 0.0f;
    out[0] = 0.0f;
    _mlir_ciface_semi_affine_fill_and_sum(stride0, stride1, &flat_ref, &out_ref);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  double total_ns = (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
                    (double)(end.tv_nsec - start.tv_nsec);
  const float expected = 256.0f * 1024.0f;
  if (out[0] != expected) {
    fprintf(stderr, "unexpected checksum: got %.1f expected %.1f\n", out[0], expected);
    free(flat);
    free(out);
    return 3;
  }

  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  printf("result=%.1f\n", out[0]);
  printf("expected_result=%.1f\n", expected);
  printf("iterations=%lld\n", (long long)iterations);
  printf("total_ns=%.0f\n", total_ns);
  printf("ns_per_iter=%.2f\n", total_ns / (double)iterations);

  free(flat);
  free(out);
  return 0;
}
