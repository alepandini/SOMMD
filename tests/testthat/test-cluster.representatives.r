test_that("cluster.representatives works correctly", {
  # Load SOM
  som_file <- system.file("extdata", "SOM.rds", package = "SOMMD")
  som_model <- readRDS(som_file)

  #Compute clusters
  som_cluster <- stats::cutree(stats::hclust(stats::dist(som_model$codes[[1]], method="euclidean"), method="complete"), 4)
  
  # Execute Function
  result <- cluster.representatives(som_model, som_cluster)
  
  #Verify the obtained frames
  expected_frames <- c(97,4,18,140)
  names(expected_frames) <- c("A", "B", "C", "D")
  expect_equal(result$frames, expected_frames, tolerance = 1e-6)

  #Verify the obtained neurons
  expected_neurons <- c(13, 5, 15, 17)
  names(expected_neurons) <- c("A", "B", "C", "D")
  expect_equal(result$neurons, expected_neurons, tolerance = 1e-6)
})

