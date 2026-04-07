#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "control_flow_selected_subview_reduction"
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

#ifdef MLIR_C_INTERFACE
extern void _mlir_ciface_control_flow_selected_subview_reduction(
    _Bool sel, MemRef1D_f32 *flat, MemRef1D_f32 *out);
#else
extern void control_flow_selected_subview_reduction(
    _Bool sel, MemRef1D_f32 *flat, MemRef1D_f32 *out);
#endif

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
  const int64_t total = 20;
  int64_t sel = 0;
  int64_t iterations = 10000;

  if (argc > 1) sel = strtoll(argv[1], NULL, 10);
  if (argc > 2) iterations = strtoll(argv[2], NULL, 10);
  if (sel < 0 || sel > 1) {
    fprintf(stderr, "selector must be 0 or 1\n");
    return 1;
  }
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

  for (int64_t i = 0; i < total; ++i) flat[i] = (float)(i + 1);
  out[0] = 0.0f;

  MemRef1D_f32 flat_ref = make1D(flat, total);
  MemRef1D_f32 out_ref = make1D(out, 1);

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    out[0] = 0.0f;
#ifdef MLIR_C_INTERFACE
    _mlir_ciface_control_flow_selected_subview_reduction(sel == 0, &flat_ref, &out_ref);
#else
    control_flow_selected_subview_reduction(sel == 0, &flat_ref, &out_ref);
#endif
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  double total_ns = (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
                    (double)(end.tv_nsec - start.tv_nsec);
  float expected = sel == 0 ? 36.0f : 64.0f;
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
  printf("selector=%lld\n", (long long)sel);
  printf("iterations=%lld\n", (long long)iterations);
  printf("total_ns=%.0f\n", total_ns);
  printf("ns_per_iter=%.2f\n", total_ns / (double)iterations);

  free(flat);
  free(out);
  return 0;
}
