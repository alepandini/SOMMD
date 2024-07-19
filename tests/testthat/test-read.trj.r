library(testthat)

test_that("read.trj reads trajectory and topology files correctly", {
  topfile <- system.file("extdata", "HIF2a.gro", package = "SOMMD")
  trjfile <- system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD")
  
  # Read the trajectory and topology
  trj <- read.trj(trjfile = trjfile, topfile = topfile)
  
  # Check if the returned object is of class "trj"
  expect_s3_class(trj, "trj")
  
  # Check the components of the trj object
  expect_named(trj, c("topfile", "topformat", "trjfile", "trjformat", "coord", "frameidx", "top", "start", "end", "call"))
  
  # Verify the dimension of coordinates
  result <- dim(trj$coord)
  expected_result <- c(1809, 3, 25)
  expect_equal(result, expected_result, tolerance = 1e-6)
  
  # Check that the topology contains the required columns
  expect_named(trj$top, c("resno", "resid", "elety", "eleno", "chain"))

  # Verify the number of atoms in the topology and trajectory
  expect_equal(nrow(trj$top), nrow(trj$coord))
  
  #Verify the coordinates of an atom in trajectory
  result <- trj$coord[1809,,12]
  expected_result <- c(5.09, 6.20, 4.19)
  expect_equal(result, expected_result, tolerance = 1e-6)

})

