test_that("read.gro correctly parses .gro file", {
  # Read gro file
  gro_file <- system.file("extdata/", "HIF2a.gro", package = "SOMMD")
  gro <- read.gro(gro_file)
  
  # Check the class of the output
  expect_s3_class(gro, "gro")
  
  # Check that output are correct
  expect_true(is.data.frame(gro$atom))
  expect_true(is.matrix(gro$xyz))
  expect_true(is.numeric(gro$box))
  
  # Check colnames
  expected_colnames <- c("resno", "resid", "elety", "eleno", "x", "y", "z", "Vx", "Vy", "Vz")
  expect_equal(colnames(gro$atom), expected_colnames)
  
  # Check format of the data
  expect_true(all(sapply(gro$atom[, c("resno", "eleno", "x", "y", "z", "Vx", "Vy", "Vz")], is.numeric)))
  expect_true(all(sapply(gro$atom[, c("resid", "elety")], is.character)))

  # Check coordinates of an atom
  result <- as.character(gro$atom[100,c(1:7)])
  expected_result <- c(244, "PHE", "O", 100, 5.287, 7.315, 3.876)
  expect_equal(result, expected_result, tolerance = 1e-6)

})

