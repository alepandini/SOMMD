test_that("normalize matrix in two approach iso ", {
  mat1 <- matrix(c(0,1,2,0,1,2), nrow = 2 , ncol = 3)
  normMat <- matrix(c(0,0.5, 1,0 , 0.5 ,1), nrow = 2 , ncol = 3)
  calcNorm <- scaler(mat = mat1, method = "minmax", by = "whole")
  print(calcNorm)
  expect_equal(calcNorm[1], normMat[1])

})
