#' @export
read.trj <- function(trjfile, topfile){
  supported_trj_formats <- c("dcd","xtc")

  trjfileExtension <- tools::file_ext(trjfile)
  topfileExtension <- tools::file_ext(topfile)

  if(!trjfileExtension %in% supported_trj_formats){
    stop("SOMMD currely supports only .dcd, .xtc or .ncdf trajectory formats.")
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
  trj$trjfile <- trjfile
  trj$trjformat <- trjfileExtension
  trj$topformat <- topfileExtension
  trj$topfile <- topfile
  trj$coord <- trj_coord
  trj$top <- data.frame(0)
  trj$start <- c(0)
  trj$end<- c(0)
  trj$call <- ""

  class(trj) <- "trj"

  return(trj)
}
