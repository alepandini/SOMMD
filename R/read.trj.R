#' @export
read.trj <- function(trjfile, topfile){
  supported_top_formats <- c("pdb","gro")
  supported_trj_formats <- c("dcd","xtc")

  topfileExtension <- tools::file_ext(topfile)
  trjfileExtension <- tools::file_ext(trjfile)

  if(!topfileExtension %in% supported_top_formats){
    stop("SOMMD currely does not support this topology format.")
  }

  if(!trjfileExtension %in% supported_trj_formats){
    stop("SOMMD currely does not support this trajectory format.")
  }

  if(topfileExtension == "pdb"){
    top_pdb <- bio3d::read.pdb(topfile, verbose = F)
    pdb_columns <- c("resno", "resid", "elety", "eleno", "chain")
    top <- top_pdb$atom[,pdb_columns]
  }

  if(topfileExtension == "gro"){
    top_gro <- read.gro(topfile)
    top_gro$atom$chain <- NA
    gro_columns <- c("resno", "resid", "elety", "eleno", "chain")
    top <- top_gro$atom[,gro_columns]
  }

  if(trjfileExtension == "dcd"){
    trj_dcd <- bio3d::read.dcd(trjfile, verbose = F)
    nframes <- dim(trj_dcd)[1]
    ncoords <- dim(trj_dcd)[2]
    natoms <- ncoords/3
    trj_dcd[,] <- t(trj_dcd[,])
    dim(trj_dcd) <- c(nframes * ncoords, 1)
    dim(trj_dcd) <- c(3, natoms, nframes)
    trj_coord <- aperm(trj_dcd, c(2,1,3))
  }

  if(trjfileExtension == "xtc"){
    trj_xtc <- rio_read_xtc(trjfile)
    trj_coord <- trj_xtc
  }

  trj <- NULL

  trj$topfile <- topfile
  trj$topformat <- topfileExtension
  trj$trjfile <- trjfile
  trj$trjformat <- trjfileExtension
  trj$coord <- trj_coord
  trj$top <- top
  trj$start <- c(0)
  trj$end<- c(0)
  trj$call <- sys.call()

  class(trj) <- "trj"

  return(trj)
}
