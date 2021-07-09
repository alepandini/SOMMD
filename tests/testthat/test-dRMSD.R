test_that("dRMSD Calculates distance root-mean-square deviation", {
  vec1 <- as.vector( c(1 , 2 , 3 , 4 , 5))
  vec2 <- as.vector(c (5 , 4 , 3 , 2 , 1))
  test_drmsd <- sqrt((1/ (5 * 4)) * sum((vec1 - vec2) ^ 2) )


  expect_equal(dRMSD(vec1 , vec2 , 5) , test_drmsd)
})
