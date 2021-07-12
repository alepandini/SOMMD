test_that("Calculating the Distance Matrix", {
  VEC <- as.vector (1,2,3,4,5,6,7,8,9,10)
  N_atm <- (length(VEC)-1)/3
  mat <- matrix(VEC[2:length(VEC)], ncol=N_atm)
  transposed_mat <- t(mat)
  dist_mat <- dist(transposed_mat, method='euclidean', upper=TRUE, diag=TRUE)

  expect_equal(Calc_dist_mat(VEC), dist_mat)
})





