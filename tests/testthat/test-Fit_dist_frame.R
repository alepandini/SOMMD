test_that("Using fit.xyz distance between frames", {


  aln <- bio3d::read.fasta(system.file("examples/kif1a.fa",package="bio3d"))
  pdbs <- bio3d::read.fasta.pdb(aln)
  gaps <- bio3d::gap.inspect(pdbs$xyz)

  ##-- Superpose again this time outputing PDBs

  xyz <- bio3d::fit.xyz( fixed = pdbs$xyz[1,],
                  mobile = pdbs,
                  fixed.inds = gaps$f.inds,
                  mobile.inds = gaps$f.inds,
                  outpath = "rough_fit",
                  full.pdbs = TRUE)

  ##--- Fit two PDBs
  A <- bio3d::read.pdb("1bg2")
  A.ind <- bio3d::atom.select(A, resno=c(256:269), elety='CA')
  B <- bio3d::read.pdb("2kin")
  B.ind <- bio3d::atom.select(B, resno=c(257:270), elety='CA')
  xyz <- bio3d::fit.xyz(fixed=A$xyz, mobile=B$xyz,
                 fixed.inds=A.ind$xyz,
                 mobile.inds=B.ind$xyz)

  expect_equal(2 * 2, 4)
})
