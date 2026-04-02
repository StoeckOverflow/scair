#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
  float *allocated;
  float *aligned;
  int64_t offset;
  int64_t sizes[1];
  int64_t strides[1];
} MemRef1D_f32;

extern void semi_affine_fill_and_sum(
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

int main(void) {
  const int64_t stride0 = 1024;
  const int64_t stride1 = 1;
  const int64_t total = 256 * stride0;

  float *flat = (float *)malloc((size_t)total * sizeof(float));
  float *out = (float *)malloc(sizeof(float));

  if (!flat || !out) {
    fprintf(stderr, "allocation failed\n");
    free(flat);
    free(out);
    return 2;
  }

  for (int64_t i = 0; i < total; ++i) flat[i] = 0.0f;
  out[0] = 0.0f;

  MemRef1D_f32 flat_ref = make1D(flat, total);
  MemRef1D_f32 out_ref = make1D(out, 1);

  semi_affine_fill_and_sum(stride0, stride1, &flat_ref, &out_ref);

  printf("checksum = %.1f\n", out[0]);

  free(flat);
  free(out);
  return 0;
}
