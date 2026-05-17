#ifndef SEMI_AFFINE_DRIVER_COMMON_H
#define SEMI_AFFINE_DRIVER_COMMON_H

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define SEMI_AFFINE_DEFAULT_ROWS 256
#define SEMI_AFFINE_DEFAULT_COLS 1024
#define SEMI_AFFINE_DEFAULT_STRIDE1 2

typedef struct {
  int64_t iterations;
  int64_t stride0;
  int64_t stride1;
  int64_t rows;
  int64_t cols;
  int64_t total;
  float expected;
} SemiAffineConfig;

static int parse_i64_arg(const char *text, const char *name, int64_t *out) {
  char *end = NULL;
  errno = 0;
  long long value = strtoll(text, &end, 10);
  if (errno != 0 || end == text || *end != '\0') {
    fprintf(stderr, "invalid %s: %s\n", name, text);
    return 0;
  }
  *out = (int64_t)value;
  return 1;
}

static int checked_mul_i64(int64_t lhs, int64_t rhs, int64_t *out) {
  if (lhs < 0 || rhs < 0) return 0;
  if (lhs != 0 && rhs > INT64_MAX / lhs) return 0;
  *out = lhs * rhs;
  return 1;
}

static int checked_add_i64(int64_t lhs, int64_t rhs, int64_t *out) {
  if (lhs < 0 || rhs < 0) return 0;
  if (rhs > INT64_MAX - lhs) return 0;
  *out = lhs + rhs;
  return 1;
}

static int validate_layout_is_injective(const SemiAffineConfig *cfg) {
  if ((uint64_t)cfg->total > (uint64_t)SIZE_MAX) {
    fprintf(stderr, "layout backing size is too large for this platform\n");
    return 0;
  }

  unsigned char *seen = (unsigned char *)calloc((size_t)cfg->total, 1);
  if (!seen) {
    fprintf(stderr, "layout validation allocation failed\n");
    return 0;
  }

  for (int64_t i = 0; i < cfg->rows; ++i) {
    for (int64_t j = 0; j < cfg->cols; ++j) {
      int64_t row_offset;
      int64_t col_offset;
      int64_t index;
      if (!checked_mul_i64(i, cfg->stride0, &row_offset) ||
          !checked_mul_i64(j, cfg->stride1, &col_offset) ||
          !checked_add_i64(row_offset, col_offset, &index) ||
          index < 0 || index >= cfg->total) {
        fprintf(stderr, "layout index overflow during validation\n");
        free(seen);
        return 0;
      }
      if (seen[index]) {
        fprintf(stderr,
                "layout aliases logical elements at physical index %lld\n",
                (long long)index);
        free(seen);
        return 0;
      }
      seen[index] = 1;
    }
  }

  free(seen);
  return 1;
}

static int parse_semi_affine_config(int argc, char **argv,
                                    SemiAffineConfig *cfg) {
  cfg->iterations = 100;
  cfg->rows = SEMI_AFFINE_DEFAULT_ROWS;
  cfg->cols = SEMI_AFFINE_DEFAULT_COLS;
  cfg->stride1 = SEMI_AFFINE_DEFAULT_STRIDE1;
  cfg->stride0 = SEMI_AFFINE_DEFAULT_COLS * SEMI_AFFINE_DEFAULT_STRIDE1;

  if (argc != 1 && argc != 2 && argc != 4 && argc != 6) {
    fprintf(stderr,
            "usage: %s [iterations [stride0 stride1 [rows cols]]]\n",
            argv[0]);
    return 0;
  }
  if (argc >= 2 &&
      !parse_i64_arg(argv[1], "iterations", &cfg->iterations)) {
    return 0;
  }
  if (argc >= 4 &&
      (!parse_i64_arg(argv[2], "stride0", &cfg->stride0) ||
       !parse_i64_arg(argv[3], "stride1", &cfg->stride1))) {
    return 0;
  }
  if (argc >= 6 &&
      (!parse_i64_arg(argv[4], "rows", &cfg->rows) ||
       !parse_i64_arg(argv[5], "cols", &cfg->cols))) {
    return 0;
  }

  if (cfg->iterations <= 0 || cfg->rows <= 0 || cfg->cols <= 0 ||
      cfg->stride0 <= 0 || cfg->stride1 <= 0) {
    fprintf(stderr,
            "iterations, rows, cols, stride0, and stride1 must be positive\n");
    return 0;
  }

  int64_t expected_i64;
  int64_t row_max;
  int64_t col_max;
  int64_t max_index;
  if (!checked_mul_i64(cfg->rows, cfg->cols, &expected_i64) ||
      !checked_mul_i64(cfg->rows - 1, cfg->stride0, &row_max) ||
      !checked_mul_i64(cfg->cols - 1, cfg->stride1, &col_max) ||
      !checked_add_i64(row_max, col_max, &max_index) ||
      !checked_add_i64(max_index, 1, &cfg->total)) {
    fprintf(stderr, "semi-affine layout arithmetic overflow\n");
    return 0;
  }
  if ((uint64_t)cfg->total > (uint64_t)SIZE_MAX / sizeof(float)) {
    fprintf(stderr, "semi-affine allocation size overflow\n");
    return 0;
  }

  cfg->expected = (float)expected_i64;
  return validate_layout_is_injective(cfg);
}

static void print_semi_affine_config(const SemiAffineConfig *cfg) {
  printf("rows=%lld\n", (long long)cfg->rows);
  printf("cols=%lld\n", (long long)cfg->cols);
  printf("stride0=%lld\n", (long long)cfg->stride0);
  printf("stride1=%lld\n", (long long)cfg->stride1);
  printf("total=%lld\n", (long long)cfg->total);
}

#endif
