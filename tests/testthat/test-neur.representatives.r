test_that("neur.representatives works correctly", {
  # Load SOM
  som_file <- system.file("extdata", "SOM.rds", package = "SOMMD")
  som_model <- readRDS(som_file)

  # Execute Function
  result <- neur.representatives(som_model)
  
  #Verify the obtained frames
  expected_frames <-  c(73, 69, 81, 42, 4, 129, 102, 95, 65, 8, 117, 139, 97, NA, 18, 137, 140, 52, 86, 49, 118, 108, 76, NA, 34)
  expect_equal(result, expected_frames, tolerance = 1e-6)

})

