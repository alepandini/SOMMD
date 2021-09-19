test_that("Reading Reference file with pdb Extension", {
  pathFile <- "../SOMMD/data/ref.pdb"
  err <- "read_pdb: the specified file is not in pdb format or pdb index"
  sampleFile <- bio3d::read.pdb(pathFile)
  testFile <- read_pdb(pathFile)
  atomTableDimension <- dim(sampleFile$atom)
  expect_identical(dim(testFile$atom), atomTableDimension )
  expect_error(read_pdb("../SOMMD/data/REP_001.xtc"), err)
})
