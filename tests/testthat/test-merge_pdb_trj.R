test_that("Merging a pdb and a trajectory file ", {

  pdb_file <- bio3d::read.pdb( system.file("examples/1hel.pdb", package="bio3d") )
  trj_file <- bio3d::read.dcd( system.file("examples/hivp.dcd", package="bio3d") )


  output_merge <- rbind(pdb_file$xyz , trj_file$xyz)

  expect_equal(dim(output_merge), dim(Merge_pdb_trj(pdb_file,trj_file)$xyz))
})
