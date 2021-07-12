test_that("Calculating the Distance Matrix", {
  a <- dm.VEC(pdbs$xyz[1,])
  b <- dm.VEC(pdbs$xyz[2,])
  N_atm <- (length(VEC)-1)/3
  MAT <- as.matrix((dist(t(matrix(VEC[2:length(VEC)], ncol=N_atm)), method='euclidean', upper=TRUE, diag=TRUE)))
  return(VEC)
})
