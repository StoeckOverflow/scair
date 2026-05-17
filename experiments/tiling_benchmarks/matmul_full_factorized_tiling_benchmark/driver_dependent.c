#include "driver_common.h"

extern void matmul_full_factorized_tiling(
    int64_t m0_nat, int64_t m1_nat, int64_t n0_nat, int64_t n1_nat,
    int64_t k0_nat, int64_t k1_nat, float *A, float *B, float *C);

int main(int argc, char **argv) {
  int64_t iterations = 100;
  if (argc > 1) iterations = strtoll(argv[1], NULL, 10);
  if (iterations <= 0) return 1;

  float *A = (float *)malloc((size_t)kAElements * sizeof(float));
  float *B = (float *)malloc((size_t)kBElements * sizeof(float));
  float *C = (float *)malloc((size_t)kCElements * sizeof(float));
  float *Ref = (float *)malloc((size_t)kCElements * sizeof(float));
  if (!A || !B || !C || !Ref) return 2;

  init_inputs(A, B);
  fill(C, kCElements, 0.0f);
  fill(Ref, kCElements, 0.0f);
  reference_matmul(A, B, Ref);
  const float expected = checksum(Ref, kCElements);

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    fill(C, kCElements, 0.0f);
    matmul_full_factorized_tiling(kM0, kM1, kN0, kN1, kK0, kK1, A, B, C);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  const float result = checksum(C, kCElements);
  const int ok = verify_close(C, Ref, kCElements);
  const double ns = elapsed_ns(start, end) / (double)iterations;
  printf("run_status=%s\n", ok ? "ok" : "fail");
  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  printf("result=%.9g\n", result);
  printf("expected_result=%.9g\n", expected);
  printf("checksum=%.9g\n", result);
  printf("iterations=%lld\n", (long long)iterations);
  printf("total_ns=%.0f\n", elapsed_ns(start, end));
  printf("ns_per_iter=%.2f\n", ns);

  free(A);
  free(B);
  free(C);
  free(Ref);
  return ok ? 0 : 3;
}
