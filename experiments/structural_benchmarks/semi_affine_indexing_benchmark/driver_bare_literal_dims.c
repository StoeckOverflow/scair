#define _POSIX_C_SOURCE 199309L

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "driver_common.h"

#ifndef BENCH_LABEL
#define BENCH_LABEL "semi_affine_fill_and_sum"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "unknown"
#endif

extern void semi_affine_fill_and_sum(
    int64_t rows_size, int64_t cols_size, int64_t stride0, int64_t stride1,
    float *flat, float *out);

int main(int argc, char **argv) {
  SemiAffineConfig cfg;
  if (!parse_semi_affine_config(argc, argv, &cfg)) {
    return 1;
  }

  float *flat = (float *)malloc((size_t)cfg.total * sizeof(float));
  float *out = (float *)malloc(sizeof(float));

  if (!flat || !out) {
    fprintf(stderr, "allocation failed\n");
    free(flat);
    free(out);
    return 2;
  }

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < cfg.iterations; ++iter) {
    for (int64_t i = 0; i < cfg.total; ++i) flat[i] = 0.0f;
    out[0] = 0.0f;
    semi_affine_fill_and_sum(cfg.rows, cfg.cols, cfg.stride0, cfg.stride1, flat, out);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  double total_ns = (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
                    (double)(end.tv_nsec - start.tv_nsec);
  if (out[0] != cfg.expected) {
    fprintf(stderr, "unexpected checksum: got %.1f expected %.1f\n", out[0], cfg.expected);
    free(flat);
    free(out);
    return 3;
  }

  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  print_semi_affine_config(&cfg);
  printf("result=%.1f\n", out[0]);
  printf("expected_result=%.1f\n", cfg.expected);
  printf("iterations=%lld\n", (long long)cfg.iterations);
  printf("total_ns=%.0f\n", total_ns);
  printf("ns_per_iter=%.2f\n", total_ns / (double)cfg.iterations);

  free(flat);
  free(out);
  return 0;
}
