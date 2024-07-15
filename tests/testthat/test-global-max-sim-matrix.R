test_that("compute global max similarity matrix integration test", {
  # given
  d <- c("match" = 1, "mismatch" = -1, "gap" = -2)

  # when
  test_mat_b <- init_matrix_col1(matrix(0, 3+1, 3+1), 3)
  test_mat_c <- init_matrix_row1(test_mat_b, 3)
  test_mat_c <- init_matrix_row1(test_mat_b, 3)
  result <- populate_global_max_sim_matrix(test_mat_c, 'AAG', 'ACC', d, 3, 3)

  # then
  expected <- rbind(c(0, -2, -4, -6), c(-2, 1, -1, -3), c(-4, -1, 0, -2), c(-6, -3, -2, -1))
  print(expected)
  print(result)
  expect_identical(expected, result)
})
