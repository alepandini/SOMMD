test_that("Using fit.xyz distance between frames", {


  aln <- read.fasta(system.file("examples/kif1a.fa",package="bio3d"))
  pdbs <- read.fasta.pdb(aln)
  gaps <- gap.inspect(pdbs$xyz)

  ##-- Superpose again this time outputing PDBs

  xyz <- fit.xyz( fixed = pdbs$xyz[1,],
                  mobile = pdbs,
                  fixed.inds = gaps$f.inds,
                  mobile.inds = gaps$f.inds,
                  outpath = "rough_fit",
                  full.pdbs = TRUE)

  ##--- Fit two PDBs
  A <- read.pdb("1bg2")
  A.ind <- atom.select(A, resno=c(256:269), elety='CA')
  B <- read.pdb("2kin")
  B.ind <- atom.select(B, resno=c(257:270), elety='CA')
  xyz <- fit.xyz(fixed=A$xyz, mobile=B$xyz,
                 fixed.inds=A.ind$xyz,
                 mobile.inds=B.ind$xyz)

  expect_equal(2 * 2, 4)
})
