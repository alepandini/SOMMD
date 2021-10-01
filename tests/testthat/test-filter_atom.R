test_that("Filter atoms in pdb object", {
load("../SOMMD/data/pdb_and_tarj_object.RData")
 merged_pdb <-  merge_pdb_trj(pdb1 , trj1)
 ## pdb outputs
 cb_pdb <-  fliter(merged_pdb , atom = "CB", output_type = "pdb")

 ## coordinates output
 cb_coordinates <-  fliter(merged_pdb , atom = "CB", output_type = "xyz")

  expect_equal(cb_pdb$xyz , cb_coordinates)
})
