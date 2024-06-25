test_that("struct2pdb works correctly", {
  # Load gro file
  gro_file <- system.file("extdata", "THS020.gro", package = "SOMMD")
  gro <- read.struct(gro_file)

  # Execute Function
  pdb <- struct2pdb(gro)
  
  # Check atoms in pdb and gro are equal
  atoms <- pdb$atom[,c(2,3,5,7)]
  expected_atoms <- gro$atom[,c(1,2,3,5)]
  expect_equal(atoms, expected_atoms, tolerance = 1e-6)

  # Check that coordinates are consistent
  coords <- pdb$atom[,c(9:11)]
  expected_coords <- gro$atom[,c(6:8)]*10
  expect_equal(coords, expected_coords, tolerance = 1e-6)
  
})

