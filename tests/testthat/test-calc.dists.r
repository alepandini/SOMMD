test_that("calc.dists computes distances correctly", {
  coord <- matrix(c(0,0,0, 1,0,0, 0,1,0, 1,1,0), nrow = 4, byrow = TRUE)
  
  # Test Complete distance Matrix calculation
  result <- calc.dists(coord)
  expected_result <- as.matrix(dist(coord, method = 'euclidean', upper = TRUE, diag = TRUE))
  expect_equal(result, c(expected_result))
  
  # Test intermolecular distance calculation
  result <- calc.dists(coord, mol.1_id = c(1, 2), mol.2_id = c(3, 4))
  expected_result <- as.matrix(dist(coord, method = 'euclidean', upper = TRUE, diag = TRUE))[c(1, 2), c(3, 4)]
  expect_equal(result, c(expected_result))
  
  # Test selection of distances
  sele <- c(1, 3, 4)
  result <- calc.dists(coord, sele = sele)
  expected_result <- as.matrix(dist(coord, method = 'euclidean', upper = TRUE, diag = TRUE))[sele]
  expect_equal(result, expected_result)
})
