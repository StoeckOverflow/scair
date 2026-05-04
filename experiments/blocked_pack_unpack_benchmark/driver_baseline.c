#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "blocked_pack"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "scair_baseline"
#endif

#ifndef BLOCKED_PACK_MO
#define BLOCKED_PACK_MO 64
#endif

#ifndef BLOCKED_PACK_NO
#define BLOCKED_PACK_NO 64
#endif

#ifndef BLOCKED_PACK_TM
#define BLOCKED_PACK_TM 16
#endif

#ifndef BLOCKED_PACK_TN
#define BLOCKED_PACK_TN 16
#endif

enum {
  kMo = BLOCKED_PACK_MO,
  kNo = BLOCKED_PACK_NO,
  kTm = BLOCKED_PACK_TM,
  kTn = BLOCKED_PACK_TN,
  kM = kMo * kTm,
  kN = kNo * kTn,
  kElements = kM * kN,
};

typedef struct {
  int64_t *allocated;
  int64_t *aligned;
  int64_t offset;
  int64_t sizes[1];
  int64_t strides[1];
} MemRef1D_i64;

extern void blocked_pack(
    int64_t mo, int64_t no, int64_t tm, int64_t tn,
    MemRef1D_i64 *src, MemRef1D_i64 *dst);

static double elapsed_ns(struct timespec start, struct timespec end) {
  return (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
         (double)(end.tv_nsec - start.tv_nsec);
}

static MemRef1D_i64 make1D(int64_t *ptr, int64_t n) {
  MemRef1D_i64 m;
  m.allocated = ptr;
  m.aligned = ptr;
  m.offset = 0;
  m.sizes[0] = n;
  m.strides[0] = 1;
  return m;
}

static void init_src(int64_t *src) {
  for (int64_t i = 0; i < kElements; ++i) {
    src[i] = (int64_t)((i * 1315423911ull + 17ull) & 0x7fff);
  }
}

static void fill(int64_t *ptr, int64_t n, int64_t value) {
  for (int64_t i = 0; i < n; ++i) ptr[i] = value;
}

static int64_t packed_index(int64_t mo_i, int64_t no_i, int64_t mi, int64_t ni) {
  return (((mo_i * kNo + no_i) * kTm + mi) * kTn + ni);
}

static void reference_pack(const int64_t *src, int64_t *ref) {
  for (int64_t mo_i = 0; mo_i < kMo; ++mo_i) {
    for (int64_t no_i = 0; no_i < kNo; ++no_i) {
      for (int64_t mi = 0; mi < kTm; ++mi) {
        int64_t row = mo_i * kTm + mi;
        for (int64_t ni = 0; ni < kTn; ++ni) {
          int64_t col = no_i * kTn + ni;
          ref[packed_index(mo_i, no_i, mi, ni)] = src[row * kN + col];
        }
      }
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

  int64_t *src = (int64_t *)malloc((size_t)kElements * sizeof(int64_t));
  int64_t *dst = (int64_t *)malloc((size_t)kElements * sizeof(int64_t));
  int64_t *ref = (int64_t *)malloc((size_t)kElements * sizeof(int64_t));
  if (!src || !dst || !ref) {
    fprintf(stderr, "allocation failed\n");
    free(src);
    free(dst);
    free(ref);
    return 2;
  }

  init_src(src);
  fill(dst, kElements, 0);
  reference_pack(src, ref);
  const int64_t expected = checksum(ref, kElements);

  MemRef1D_i64 src_ref = make1D(src, kElements);
  MemRef1D_i64 dst_ref = make1D(dst, kElements);

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    blocked_pack(kMo, kNo, kTm, kTn, &src_ref, &dst_ref);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  const int64_t result = checksum(dst, kElements);
  if (!verify_equal(dst, ref, kElements)) {
    fprintf(stderr, "unexpected blocked_pack result\n");
    printf("run_status=fail\n");
    printf("result=%lld\n", (long long)result);
    printf("expected_result=%lld\n", (long long)expected);
    printf("ns_per_iter=%.2f\n", elapsed_ns(start, end) / (double)iterations);
    free(src);
    free(dst);
    free(ref);
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

  free(src);
  free(dst);
  free(ref);
  return 0;
}
