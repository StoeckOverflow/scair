#include <math.h>
#include <stdint.h>

float bench_expf(float x) { return expf(x); }

float bench_inv_sqrt_index(int64_t x) {
  return 1.0f / sqrtf((float)x);
}
