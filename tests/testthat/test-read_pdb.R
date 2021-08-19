test_that("Reading Reference file with pdb Extension", {

  err <- "read_pdb: the specified file is not in pdb format or pdb index"
  expect_error(SOMMD::read_pdb("../SOMMD/data/REP_001.xtc"), err)
  expect_identical(bio3d::read.pdb("./data/ref.pdb"),
                   read_pdb("./data/ref.pdb"))
})
