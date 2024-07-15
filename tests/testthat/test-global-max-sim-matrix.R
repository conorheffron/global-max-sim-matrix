test_that("test init_matrix_col1 works", {
  # given
  input_mat <- matrix(0, 3+1, 3+1)

  # when
  result <- init_matrix_col1(input_mat, 3)

  # then
  expected <- rbind(c(0, 0, 0, 0),
                    c(-2, 0, 0, 0),
                    c(-4, 0, 0, 0),
                    c(-6, 0, 0, 0))
  expect_identical(expected, result)
})

test_that("test init_matrix_row1 works", {
  # given
  input_mat <- matrix(0, 3+1, 3+1)

  # when
  result <- init_matrix_row1(input_mat, 3)

  # then
  expected <- rbind(c(0, -2, -4, -6),
                    c(0, 0, 0, 0),
                    c(0, 0, 0, 0),
                    c(0, 0, 0, 0))
  expect_identical(expected, result)
})

test_that("test populate_global_max_sim_matrix works", {
  # given
  d <- c("match" = 1, "mismatch" = -1, "gap" = -2)
  m <- 3
  n <- 3
  seq1 <- 'AAG'
  seq2 <- 'ACC'
  input_mat <- matrix(0, m+1, n+1)

  # when
  test_mat_a <- init_matrix_col1(input_mat, m)
  test_mat_b <- init_matrix_row1(test_mat_a, n)
  result <- populate_global_max_sim_matrix(test_mat_b, seq1, seq2, d, m, n)

  # then
  expected <- rbind(c(0, -2, -4, -6),
                    c(-2, 1, -1, -3),
                    c(-4, -1, 0, -2),
                    c(-6, -3, -2, -1))
  expect_identical(expected, result)
})


test_that("test populate_global_max_sim_matrix works m by n is 15x15", {
  # given
  d <- c("match" = 1, "mismatch" = -1, "gap" = -2)
  seq1 <- 'AAGTGCCTCAAGATA'
  seq2 <- 'ACCGTCTCAGCAATA'
  m <- nchar(seq1)
  n <- nchar(seq2)
  input_mat <- matrix(0, m+1, n+1)

  # when
  input_mat <- init_matrix_col1(input_mat, m)
  input_mat <- init_matrix_row1(input_mat, n)
  result <- populate_global_max_sim_matrix(input_mat, seq1, seq2, d, m, n)

  # then
  expected <- rbind(c(0, -2, -4, -6, -8, -10, -12, -14, -16, -18, -20, -22, -24, -26, -28, -30),
                    c(-2, 1, -1, -3, -5, -7, -9, -11, -13, -15, -17, -19, -21, -23, -25, -27),
                    c(-4, -1, 0, -2, -4, -6, -8, -10, -12, -12, -14, -16, -18, -20, -22, -24),
                    c(-6, -3, -2, -1, -1, -3, -5, -7, -9, -11, -11, -13, -15, -17, -19, -21),
                    c(-8, -5, -4, -3, -2, 0, -2, -4, -6, -8, -10, -12, -14, -16, -16, -18),
                    c(-10, -7, -6, -5, -2, -2, -1, -3, -5, -7, -7, -9, -11, -13, -15, -17),
                    c(-12, -9, -6, -5, -4, -3, -1, -2, -2, -4, -6, -6, -8, -10, -12, -14),
                    c(-14, -11, -8, -5, -6, -5, -2, -2, -1, -3, -5, -5, -7, -9, -11, -13),
                    c(-16, -13, -10, -7, -6, -5, -4, -1, -3, -2, -4, -6, -6, -8, -8, -10),
                    c(-18, -15, -12, -9, -8, -7, -4, -3, 0, -2, -3, -3, -5, -7, -9, -9),
                    c(-20, -17, -14, -11, -10, -9, -6, -5, -2, 1, -1, -3, -2, -4, -6, -8),
                    c(-22, -19, -16, -13, -12, -11, -8, -7, -4, -1, 0, -2, -2, -1, -3, -5),
                    c(-24, -21, -18, -15, -12, -13, -10, -9, -6, -3, 0, -1, -3, -3, -2, -4),
                    c(-26, -23, -20, -17, -14, -13, -12, -11, -8, -5, -2, -1, 0, -2, -4, -1),
                    c(-28, -25, -22, -19, -16, -13, -14, -11, -10, -7, -4, -3, -2, -1, -1, -3),
                    c(-30, -27, -24, -21, -18, -15, -14, -13, -12, -9, -6, -5, -2, -1, -2, 0))
  print(expected)
  print(result)
  expect_identical(expected, result)
})
