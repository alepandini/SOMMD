test_that("Filter atoms in pdb object", {
base::load("../SOMMD/data/pdb_and_tarj_object.RData")
 merged_pdb <-  pdb1
  merged_pdb$xyz <- rbind(pdb1$xyz , trj1)

  ## pdb outputs

 cb_pdb <-  filter_atoms(merged_pdb , atom = "CB", output_type = "pdb")

 ## coordinates output
 cb_coordinates <-  filter_atoms(merged_pdb , atom = "CB", output_type = "xyz")

  expect_equal(cb_pdb$xyz , cb_coordinates)
})
