#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "conv2d_checksum"
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

extern void _mlir_ciface_conv2d_dynamic(
    int64_t n, int64_t cin, int64_t h, int64_t w,
    int64_t cout, int64_t kh, int64_t kw,
    int64_t oh, int64_t ow,
    MemRef1D_f32 *Xflat,
    MemRef1D_f32 *Kflat,
    MemRef1D_f32 *Yflat);

extern void _mlir_ciface_checksum_dynamic(
    int64_t n, int64_t cout, int64_t oh, int64_t ow,
    MemRef1D_f32 *Yflat,
    MemRef1D_f32 *out);

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

int main(int argc, char **argv) {
  int64_t iterations = 10;
  if (argc < 8 || argc > 9) {
    fprintf(stderr, "usage: %s n cin h w cout kh kw [iterations]\n", argv[0]);
    return 1;
  }

  const int64_t n = strtoll(argv[1], NULL, 10);
  const int64_t cin = strtoll(argv[2], NULL, 10);
  const int64_t h = strtoll(argv[3], NULL, 10);
  const int64_t w = strtoll(argv[4], NULL, 10);
  const int64_t cout = strtoll(argv[5], NULL, 10);
  const int64_t kh = strtoll(argv[6], NULL, 10);
  const int64_t kw = strtoll(argv[7], NULL, 10);
  if (argc > 8) iterations = strtoll(argv[8], NULL, 10);
  if (iterations <= 0 || kh <= 0 || kw <= 0 || h < kh || w < kw) {
    fprintf(stderr, "invalid dimensions or iterations\n");
    return 1;
  }

  const int64_t oh = h - kh + 1;
  const int64_t ow = w - kw + 1;
  const int64_t xflat = n * cin * h * w;
  const int64_t kflat = cout * cin * kh * kw;
  const int64_t yflat = n * cout * oh * ow;
  float *X = (float *)malloc((size_t)xflat * sizeof(float));
  float *K = (float *)malloc((size_t)kflat * sizeof(float));
  float *Y = (float *)malloc((size_t)yflat * sizeof(float));
  float *out = (float *)malloc(sizeof(float));

  if (!X || !K || !Y || !out) {
    fprintf(stderr, "allocation failed\n");
    free(X);
    free(K);
    free(Y);
    free(out);
    return 2;
  }

  for (int64_t i = 0; i < xflat; ++i) X[i] = 1.0f;
  for (int64_t i = 0; i < kflat; ++i) K[i] = 1.0f;

  MemRef1D_f32 Xref = make1D(X, xflat);
  MemRef1D_f32 Kref = make1D(K, kflat);
  MemRef1D_f32 Yref = make1D(Y, yflat);
  MemRef1D_f32 Oref = make1D(out, 1);

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    for (int64_t i = 0; i < yflat; ++i) Y[i] = 0.0f;
    out[0] = 0.0f;
    _mlir_ciface_conv2d_dynamic(n, cin, h, w, cout, kh, kw, oh, ow, &Xref, &Kref, &Yref);
    _mlir_ciface_checksum_dynamic(n, cout, oh, ow, &Yref, &Oref);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  float expected = (float)(n * cout * oh * ow * cin * kh * kw);
  if (out[0] != expected) {
    fprintf(stderr, "unexpected checksum: got %.1f expected %.1f\n", out[0], expected);
    free(X);
    free(K);
    free(Y);
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

  free(X);
  free(K);
  free(Y);
  free(out);
  return 0;
}
