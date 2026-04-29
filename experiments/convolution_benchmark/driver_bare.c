#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "conv2d_kernel"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "unknown"
#endif

extern void conv2d_dynamic(
    int64_t n_nat, int64_t cin_nat, int64_t h_nat, int64_t w_nat,
    int64_t cout_nat, int64_t kh_nat, int64_t kw_nat,
    int64_t oh_nat, int64_t ow_nat,
    float *Xflat,
    float *Kflat,
    float *Yflat);

static double elapsed_ns(struct timespec start, struct timespec end) {
  return (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
         (double)(end.tv_nsec - start.tv_nsec);
}

static double checksum_host(const float *ptr, int64_t n) {
  double sum = 0.0;
  for (int64_t i = 0; i < n; ++i) sum += (double)ptr[i];
  return sum;
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
  if (!X || !K || !Y) {
    fprintf(stderr, "allocation failed\n");
    free(X);
    free(K);
    free(Y);
    return 2;
  }

  for (int64_t i = 0; i < xflat; ++i) X[i] = 1.0f;
  for (int64_t i = 0; i < kflat; ++i) K[i] = 1.0f;

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    for (int64_t i = 0; i < yflat; ++i) Y[i] = 0.0f;
    conv2d_dynamic(n, cin, h, w, cout, kh, kw, oh, ow, X, K, Y);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);
  const double result = checksum_host(Y, yflat);
  const double expected = (double)n * (double)cout * (double)oh * (double)ow * (double)cin * (double)kh * (double)kw;
  if (result != expected) {
    fprintf(stderr, "unexpected checksum: got %.1f expected %.1f\n", result, expected);
    free(X);
    free(K);
    free(Y);
    return 3;
  }

  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  printf("result=%.1f\n", result);
  printf("expected_result=%.1f\n", expected);
  printf("iterations=%lld\n", (long long)iterations);
  printf("total_ns=%.0f\n", elapsed_ns(start, end));
  printf("ns_per_iter=%.2f\n", elapsed_ns(start, end) / (double)iterations);

  free(X);
  free(K);
  free(Y);
  return 0;
}
