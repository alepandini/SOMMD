
test_that("calc.distances computes distances correctly", {
  trj <- list(coord = array(c(0,0,0, 1,0,0, 0,1,0, 1,1,0), dim = c(4, 3, 1)), top = list(elety = rep("CB", 4)))
  class(trj) <- "trj"
  
  # Test complete distance matrix calculation
  result <- calc.distances(trj)
  expected_result <- as.matrix(dist(matrix(c(0,0,0, 1,0,0, 0,1,0, 1,1,0), nrow = 4, byrow = FALSE), method = 'euclidean', upper = TRUE, diag = TRUE))
  expect_equal(result, t(c(expected_result)))
  
  # Test intermolecular distance calculation
  result <- calc.distances(trj, mol.2 = c(3, 4))
  expected_result <- as.matrix(dist(matrix(c(0,0,0, 1,0,0, 0,1,0, 1,1,0), nrow = 4, byrow = FALSE), method = 'euclidean', upper = TRUE, diag = TRUE))[c(1, 2), c(3, 4)]
  expect_equal(result, t(c(expected_result)))
  
  # Test distances selection
  sele <- c(1, 3, 4)
  result <- calc.distances(trj, sele = sele)
  expected_result <- as.matrix(dist(matrix(c(0,0,0, 1,0,0, 0,1,0, 1,1,0), nrow = 4, byrow = FALSE), method = 'euclidean', upper = TRUE, diag = TRUE))[sele]
  expect_equal(result, t(expected_result))
})

test_that("calc.distances applies capping correctly", {
  trj <- list(coord = array(c(0,0,0, 1,0,0, 0,1,0, 1,1,0), dim = c(4, 3, 1)), top = list(elety = rep("CB", 4)))
  class(trj) <- "trj"
  
  # Test without capping
  result <- calc.distances(trj)
  expected_result <- as.matrix(dist(matrix(c(0,0,0, 1,0,0, 0,1,0, 1,1,0), nrow = 4, byrow = FALSE), method = 'euclidean', upper = TRUE, diag = TRUE))
  expect_equal(result, t(c(expected_result)))
  
  # Test with capping
  cap_value <- 0.5
  result <- calc.distances(trj, cap = cap_value)
  expected_result[expected_result > cap_value] <- cap_value
  expect_equal(result, t(c(expected_result)))
})
