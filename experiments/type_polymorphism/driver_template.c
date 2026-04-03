#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_FN
#error "BENCH_FN must be defined"
#endif

#ifndef BENCH_LABEL
#define BENCH_LABEL "unknown"
#endif

extern int64_t BENCH_FN(
    int8_t i8v,
    int16_t i16v,
    int32_t i32v,
    int64_t i64v,
    float f32v,
    double f64v);

static double elapsed_ns(struct timespec start, struct timespec end) {
  return (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
         (double)(end.tv_nsec - start.tv_nsec);
}

int main(int argc, char **argv) {
  int iterations = 2000000;
  int8_t i8v = 2;
  int16_t i16v = 3;
  int32_t i32v = 4;
  int64_t i64v = 5;
  float f32v = 7.0f;
  double f64v = 8.0;

  if (argc > 1) iterations = atoi(argv[1]);
  if (argc > 2) i8v = (int8_t)atoi(argv[2]);
  if (argc > 3) i16v = (int16_t)atoi(argv[3]);
  if (argc > 4) i32v = atoi(argv[4]);
  if (argc > 5) i64v = strtoll(argv[5], NULL, 10);
  if (argc > 6) f32v = strtof(argv[6], NULL);
  if (argc > 7) f64v = strtod(argv[7], NULL);

  int64_t result = BENCH_FN(i8v, i16v, i32v, i64v, f32v, f64v);

  volatile int64_t sink = 0;
  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int i = 0; i < iterations; ++i) {
    sink += BENCH_FN(i8v, i16v, i32v, i64v, f32v, f64v);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  double total_ns = elapsed_ns(start, end);
  printf("benchmark=%s\n", BENCH_LABEL);
  printf("result=%lld\n", (long long)result);
  printf("sink=%lld\n", (long long)sink);
  printf("iterations=%d\n", iterations);
  printf("total_ns=%.0f\n", total_ns);
  printf("ns_per_iter=%.2f\n", total_ns / (double)iterations);
  return 0;
}
