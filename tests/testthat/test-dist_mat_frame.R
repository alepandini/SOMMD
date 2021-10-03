test_that("Calculating Distance Matrix ", {
  pdb <- bio3d::read.pdb( "4q21" )
  k <- bio3d::dm(pdb,inds="calpha")
  filled.contour(k, nlevels = 10)
  expect_equal(dist_mat_frame(pdb, inds = "calpha"),k)
  if(bio3d::check.utility("muscle")){

  pdbs <- bio3d::pdbaln( bio3d::get.pdb( c( "4q21", "521p"), path = tempdir() ), outfile = tempfile() )

    }})
