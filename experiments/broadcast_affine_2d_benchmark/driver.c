#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "broadcast_affine_2d"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "value_dependent"
#endif

#ifndef BROADCAST_AFFINE_K0
#define BROADCAST_AFFINE_K0 4096
#endif

#ifndef BROADCAST_AFFINE_K1
#define BROADCAST_AFFINE_K1 16
#endif

enum {
  kK0 = BROADCAST_AFFINE_K0,
  kK1 = BROADCAST_AFFINE_K1,
  kK = kK0 * kK1,
};

extern void broadcast_affine_2d(
    int64_t k0_nat, int64_t k1_nat,
    int64_t *X, int64_t *scale, int64_t *bias, int64_t *Y);

static double elapsed_ns(struct timespec start, struct timespec end) {
  return (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
         (double)(end.tv_nsec - start.tv_nsec);
}

static void init_inputs(int64_t *X, int64_t *scale, int64_t *bias) {
  for (int64_t i = 0; i < kK; ++i) {
    X[i] = (int64_t)((i % 17) - 8);
  }
  for (int64_t j = 0; j < kK1; ++j) {
    scale[j] = (int64_t)((j % 5) + 2);
    bias[j] = (int64_t)((j * 3) % 11 - 5);
  }
}

static void fill(int64_t *ptr, int64_t n, int64_t value) {
  for (int64_t i = 0; i < n; ++i) ptr[i] = value;
}

static void reference(const int64_t *X, const int64_t *scale, const int64_t *bias, int64_t *Ref) {
  for (int64_t b = 0; b < kK0; ++b) {
    for (int64_t j = 0; j < kK1; ++j) {
      int64_t i = b * kK1 + j;
      Ref[i] = X[i] * scale[j] + bias[j];
    }
  }
}

static int64_t checksum(const int64_t *ptr, int64_t n) {
  int64_t sum = 0;
  for (int64_t i = 0; i < n; ++i) sum += ptr[i];
  return sum;
}

static int verify_equal(const int64_t *got, const int64_t *expected, int64_t n) {
  for (int64_t i = 0; i < n; ++i) {
    if (got[i] != expected[i]) return 0;
  }
  return 1;
}

int main(int argc, char **argv) {
  int64_t iterations = 1000;
  if (argc > 1) iterations = strtoll(argv[1], NULL, 10);
  if (iterations <= 0) {
    fprintf(stderr, "iterations must be positive\n");
    return 1;
  }

  int64_t *X = (int64_t *)malloc((size_t)kK * sizeof(int64_t));
  int64_t *scale = (int64_t *)malloc((size_t)kK1 * sizeof(int64_t));
  int64_t *bias = (int64_t *)malloc((size_t)kK1 * sizeof(int64_t));
  int64_t *Y = (int64_t *)malloc((size_t)kK * sizeof(int64_t));
  int64_t *Ref = (int64_t *)malloc((size_t)kK * sizeof(int64_t));
  if (!X || !scale || !bias || !Y || !Ref) {
    fprintf(stderr, "allocation failed\n");
    free(X);
    free(scale);
    free(bias);
    free(Y);
    free(Ref);
    return 2;
  }

  init_inputs(X, scale, bias);
  fill(Y, kK, 0);
  reference(X, scale, bias, Ref);
  const int64_t expected = checksum(Ref, kK);

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    broadcast_affine_2d(kK0, kK1, X, scale, bias, Y);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  const int64_t result = checksum(Y, kK);
  if (!verify_equal(Y, Ref, kK)) {
    fprintf(stderr, "unexpected broadcast_affine_2d result\n");
    printf("run_status=fail\n");
    printf("result=%lld\n", (long long)result);
    printf("expected_result=%lld\n", (long long)expected);
    printf("ns_per_iter=%.2f\n", elapsed_ns(start, end) / (double)iterations);
    free(X);
    free(scale);
    free(bias);
    free(Y);
    free(Ref);
    return 3;
  }

  printf("run_status=ok\n");
  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  printf("result=%lld\n", (long long)result);
  printf("expected_result=%lld\n", (long long)expected);
  printf("checksum=%lld\n", (long long)result);
  printf("iterations=%lld\n", (long long)iterations);
  printf("ns_per_iter=%.2f\n", elapsed_ns(start, end) / (double)iterations);

  free(X);
  free(scale);
  free(bias);
  free(Y);
  free(Ref);
  return 0;
}
