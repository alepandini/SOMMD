test_that("Calculating the Distance Matrix", {
  VEC <- as.vector(c(1,2,3,4,5,6,7,8,9,10,11,12))
  N_atm <- (length(VEC))/3
  mat <- matrix(VEC[1:length(VEC)], nrow=N_atm)
  dist_mat <- as.matrix(dist(mat, method='euclidean', upper=TRUE, diag=TRUE))

  expect_equal(Calc_Dist_Mat(mat), dist_mat)
})





