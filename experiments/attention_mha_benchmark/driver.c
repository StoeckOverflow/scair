#define _POSIX_C_SOURCE 199309L

#include <float.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef BENCH_LABEL
#define BENCH_LABEL "attention_mha"
#endif

#ifndef VARIANT_LABEL
#define VARIANT_LABEL "value_dependent"
#endif

enum {
  kBatch = 1,
  kSeq = 128,
  kHeads = 12,
  kHeadDim = 64,
  kHidden = kHeads * kHeadDim,
  kQKVElements = kBatch * kSeq * kHidden,
  kScoreElements = kBatch * kHeads * kSeq * kSeq,
  kOutputElements = kBatch * kSeq * kHidden,
};

extern void attention_mha(
    int64_t batch_nat, int64_t seq_nat, int64_t heads_nat, int64_t head_dim_nat,
    float *Q, float *K, float *V, float *score, float *prob, float *tmp_out, float *out);

extern float bench_expf(float x);
extern float bench_inv_sqrt_index(int64_t x);

static void init_input(float *Q, float *K, float *V) {
  for (int64_t i = 0; i < kQKVElements; ++i) {
    Q[i] = (float)((i % 23) - 11) / 32.0f;
    K[i] = (float)(((i * 7) % 29) - 14) / 29.0f;
    V[i] = (float)(((i * 5) % 31) + 1) / 27.0f;
  }
}

static void fill(float *ptr, int64_t n, float value) {
  for (int64_t i = 0; i < n; ++i) ptr[i] = value;
}

static float checksum(const float *ptr, int64_t n) {
  float sum = 0.0f;
  for (int64_t i = 0; i < n; ++i) sum += ptr[i];
  return sum;
}

static void reference_attention_mha(
    const float *Q, const float *K, const float *V,
    float *score, float *prob, float *out) {
  const float scale = bench_inv_sqrt_index(kHeadDim);
  for (int64_t b = 0; b < kBatch; ++b) {
    for (int64_t h = 0; h < kHeads; ++h) {
      const int64_t h_base = h * kHeadDim;
      for (int64_t i = 0; i < kSeq; ++i) {
        float row_max = -FLT_MAX;
        for (int64_t j = 0; j < kSeq; ++j) {
          float sum = 0.0f;
          for (int64_t d = 0; d < kHeadDim; ++d) {
            const int64_t hd = h_base + d;
            const int64_t q_idx = ((b * kSeq + i) * kHidden) + hd;
            const int64_t k_idx = ((b * kSeq + j) * kHidden) + hd;
            sum += Q[q_idx] * K[k_idx];
          }
          const int64_t s_idx = (((b * kHeads + h) * kSeq + i) * kSeq) + j;
          score[s_idx] = sum * scale;
          if (score[s_idx] > row_max) row_max = score[s_idx];
        }
        float denom = 0.0f;
        for (int64_t j = 0; j < kSeq; ++j) {
          const int64_t s_idx = (((b * kHeads + h) * kSeq + i) * kSeq) + j;
          const float e = bench_expf(score[s_idx] - row_max);
          prob[s_idx] = e;
          denom += e;
        }
        for (int64_t j = 0; j < kSeq; ++j) {
          const int64_t s_idx = (((b * kHeads + h) * kSeq + i) * kSeq) + j;
          prob[s_idx] /= denom;
        }
      }
    }
  }

  for (int64_t b = 0; b < kBatch; ++b) {
    for (int64_t i = 0; i < kSeq; ++i) {
      for (int64_t hd = 0; hd < kHidden; ++hd) {
        const int64_t h = hd / kHeadDim;
        float sum = 0.0f;
        for (int64_t j = 0; j < kSeq; ++j) {
          const int64_t p_idx = (((b * kHeads + h) * kSeq + i) * kSeq) + j;
          const int64_t v_idx = ((b * kSeq + j) * kHidden) + hd;
          sum += prob[p_idx] * V[v_idx];
        }
        out[((b * kSeq + i) * kHidden) + hd] = sum;
      }
    }
  }
}

static int verify_close(const float *got, const float *expected, int64_t n) {
  for (int64_t i = 0; i < n; ++i) {
    const float diff = fabsf(got[i] - expected[i]);
    if (diff > 1e-4f) return 0;
  }
  return 1;
}

int main(int argc, char **argv) {
  int64_t iterations = 100;
  if (argc > 1) iterations = strtoll(argv[1], NULL, 10);
  if (iterations <= 0) {
    fprintf(stderr, "iterations must be positive\n");
    return 1;
  }

  float *Q = (float *)malloc((size_t)kQKVElements * sizeof(float));
  float *K = (float *)malloc((size_t)kQKVElements * sizeof(float));
  float *V = (float *)malloc((size_t)kQKVElements * sizeof(float));
  float *score = (float *)malloc((size_t)kScoreElements * sizeof(float));
  float *prob = (float *)malloc((size_t)kScoreElements * sizeof(float));
  float *out = (float *)malloc((size_t)kOutputElements * sizeof(float));
  float *tmp_out = (float *)malloc((size_t)kOutputElements * sizeof(float));
  float *ref_score = (float *)malloc((size_t)kScoreElements * sizeof(float));
  float *ref_prob = (float *)malloc((size_t)kScoreElements * sizeof(float));
  float *ref_out = (float *)malloc((size_t)kOutputElements * sizeof(float));
  if (!Q || !K || !V || !score || !prob || !out || !tmp_out || !ref_score || !ref_prob || !ref_out) {
    fprintf(stderr, "allocation failed\n");
    free(Q); free(K); free(V); free(score); free(prob); free(out); free(tmp_out); free(ref_score); free(ref_prob); free(ref_out);
    return 2;
  }

  init_input(Q, K, V);
  fill(score, kScoreElements, 0.0f);
  fill(prob, kScoreElements, 0.0f);
  fill(tmp_out, kOutputElements, 0.0f);
  fill(out, kOutputElements, 0.0f);
  fill(ref_score, kScoreElements, 0.0f);
  fill(ref_prob, kScoreElements, 0.0f);
  fill(ref_out, kOutputElements, 0.0f);
  reference_attention_mha(Q, K, V, ref_score, ref_prob, ref_out);
  const float expected = checksum(ref_out, kOutputElements);

  struct timespec start;
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &start);
  for (int64_t iter = 0; iter < iterations; ++iter) {
    fill(score, kScoreElements, 0.0f);
    fill(prob, kScoreElements, 0.0f);
    fill(tmp_out, kOutputElements, 0.0f);
    fill(out, kOutputElements, 0.0f);
    attention_mha(kBatch, kSeq, kHeads, kHeadDim, Q, K, V, score, prob, tmp_out, out);
  }
  clock_gettime(CLOCK_MONOTONIC, &end);

  const double total_ns = (double)(end.tv_sec - start.tv_sec) * 1000000000.0 +
                          (double)(end.tv_nsec - start.tv_nsec);
  const float result = checksum(out, kOutputElements);
  if (!verify_close(out, ref_out, kOutputElements)) {
    fprintf(stderr, "output mismatch versus reference\n");
    printf("run_status=fail\n");
    printf("result=%.9g\n", result);
    printf("expected_result=%.9g\n", expected);
    printf("ns_per_iter=%.2f\n", total_ns / (double)iterations);
    free(Q); free(K); free(V); free(score); free(prob); free(out); free(tmp_out); free(ref_score); free(ref_prob); free(ref_out);
    return 3;
  }

  printf("run_status=ok\n");
  printf("benchmark=%s\n", BENCH_LABEL);
  printf("variant=%s\n", VARIANT_LABEL);
  printf("result=%.9g\n", result);
  printf("expected_result=%.9g\n", expected);
  printf("checksum=%.9g\n", result);
  printf("iterations=%lld\n", (long long)iterations);
  printf("total_ns=%.0f\n", total_ns);
  printf("ns_per_iter=%.2f\n", total_ns / (double)iterations);

  free(Q); free(K); free(V); free(score); free(prob); free(out); free(tmp_out); free(ref_score); free(ref_prob); free(ref_out);
  return 0;
}
