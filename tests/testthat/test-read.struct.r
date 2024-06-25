test_that("read.struct correctly read gro files", {
  # Read gro file
  gro_file <- system.file("extdata/", "THS020.gro", package = "SOMMD")
  gro <- read.struct(gro_file)

  # Check the class of the output
  expect_s3_class(gro, "struct")
  
  # Check colnames
  expected_colnames <- c("eleno", "elety", "resid", "chain", "resno", "x", "y", "z", "Vx", "Vy", "Vz", "o", "b")
  expect_equal(colnames(gro$atom), expected_colnames)
  
  # Check coordinates of an atom
  result <- as.character(gro$atom[10,c(1,2,3,5,6,7,8)])
  expected_result <- c("10", "CAO", "020", "350", "5.207", "6.425", "3.406")
  expect_equal(result, expected_result)

})


test_that("read.struct correctly read pdb files", {
  # Read pdb file
  pdb_file <- system.file("extdata/", "THS020.pdb", package = "SOMMD")
  pdb <- read.struct(pdb_file)

  # Check the class of the output
  expect_s3_class(pdb, "struct")

  # Check colnames
  expected_colnames <- c("eleno", "elety", "resid", "chain", "resno", "x", "y", "z", "Vx", "Vy", "Vz", "o", "b")
  expect_equal(colnames(pdb$atom), expected_colnames)

  # Check coordinates of an atom
  result <- as.character(pdb$atom[10,c(1,2,3,5,6,7,8)])
  expected_result <- c("10", "CAO", "020", "350", "5.207", "6.425", "3.406")
  expect_equal(result, expected_result)

})

test_that("read.struct read pdb and gro files equally", {
  # Read gro file
  gro_file <- system.file("extdata/", "THS020.gro", package = "SOMMD")
  gro <- read.struct(gro_file)

  # Read pdb file
  pdb_file <- system.file("extdata/", "THS020.pdb", package = "SOMMD")
  pdb <- read.struct(pdb_file)
  
  expect_equal(pdb$atom[c("eleno", "elety", "resid", "resno", "x", "y", "z")], gro$atom[c("eleno", "elety", "resid", "resno", "x", "y", "z")], tolerance = 1e-6)
  
})
