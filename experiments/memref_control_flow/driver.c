#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_FN
#define BENCH_FN control_flow_selected_allocation_reduction
#endif

#ifndef BENCH_LABEL
#define BENCH_LABEL "control_flow_selected_allocation_reduction"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "unknown"
#endif

typedef struct {
  int64_t *allocated;
  int64_t *aligned;
  int64_t offset;
  int64_t sizes[1];
  int64_t strides[1];
} MemRef1D_i64;

#ifdef BASELINE_MEMREF_ABI
extern void BENCH_FN(_Bool sel, int64_t n, MemRef1D_i64 *out);

static MemRef1D_i64 make1D(int64_t *ptr, int64_t n) {
  MemRef1D_i64 m;
  m.allocated = ptr;
  m.aligned = ptr;
  m.offset = 0;
  m.sizes[0] = n;
  m.strides[0] = 1;
  return m;
}
#else
extern void BENCH_FN(_Bool sel, int64_t n, int64_t out_s0, int64_t *out);
#endif

static uint64_t now_ns(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
}

static int64_t expected_checksum(int64_t selector, int64_t n) {
  int64_t route0 = n * (n + 1) / 2;
  if (selector == 0) return route0;
  return 2 * route0;
}

static void run_once(int64_t selector, int64_t n, int64_t *out) {
#ifdef BASELINE_MEMREF_ABI
  MemRef1D_i64 out_ref = make1D(out, 1);
  BENCH_FN(selector == 0, n, &out_ref);
#else
  BENCH_FN(selector == 0, n, 1, out);
#endif
}

int main(int argc, char **argv) {
  int64_t selector = 0;
  int64_t n = 16;
  int64_t iterations = 10000;

  if (argc > 1) selector = strtoll(argv[1], NULL, 10);
  if (argc > 2) n = strtoll(argv[2], NULL, 10);
  if (argc > 3) iterations = strtoll(argv[3], NULL, 10);

  if (selector < 0 || selector > 1) {
    fprintf(stderr, "selector must be 0 or 1\n");
    return 1;
  }
  if (n < 0) {
    fprintf(stderr, "n must be non-negative\n");
    return 1;
  }
  if (iterations <= 0) {
    fprintf(stderr, "iterations must be positive\n");
    return 1;
  }

  int64_t *out = (int64_t *)malloc(sizeof(int64_t));
  if (!out) {
    fprintf(stderr, "allocation failed\n");
    return 2;
  }

  uint64_t start = now_ns();
  for (int64_t iter = 0; iter < iterations; ++iter) {
    out[0] = 0;
    run_once(selector, n, out);
  }
  uint64_t end = now_ns();

  int64_t expected = expected_checksum(selector, n);
  if (out[0] != expected) {
    fprintf(stderr, "unexpected checksum: got %lld expected %lld\n",
            (long long)out[0], (long long)expected);
    free(out);
    return 3;
  }

  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  printf("iterations=%lld\n", (long long)iterations);
  printf("result=%lld\n", (long long)out[0]);
  printf("expected_result=%lld\n", (long long)expected);
  printf("selector=%lld\n", (long long)selector);
  printf("n=%lld\n", (long long)n);
  printf("total_ns=%llu\n", (unsigned long long)(end - start));
  printf("ns_per_iter=%.2f\n", (double)(end - start) / (double)iterations);

  free(out);
  return 0;
}
