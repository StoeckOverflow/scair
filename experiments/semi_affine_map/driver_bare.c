#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern void semi_affine_fill_and_sum(
    int64_t stride0, int64_t stride1, int64_t flat_s0, float *flat, int64_t out_s0,
    float *out);

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

  semi_affine_fill_and_sum(stride0, stride1, total, flat, 1, out);

  printf("checksum = %.1f\n", out[0]);

  free(flat);
  free(out);
  return 0;
}
