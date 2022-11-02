#' Read trj file
#'
#' Function to read a trajectory file
#'
#' @author Alessandro Pandini
#'
#' @param trjfile contains the name and the path to the reference file (pdb or gro files are accepted)
#' @param topfile contains the name and the path to the trajectory file (xtc or dcd files are accepted)
#'
#' @return Returns a list of class "trj" with the following components:
#' @return \item{topfile}{ the input topology file.}
#' @return \item{topformat}{ the format of the input topology.}
#' @return \item{trjfile}{ the input trajectory file.}
#' @return \item{trjformat}{ the format of the input trajectory.}
#' @return \item{coord}{ a three dimensional array containing atomic coordinates for all the frames. Dimensions are: Natoms:3:Nframes.}
#' @return \item{top}{  a data.frame containing topological informations with a row per atom and a column per record type (resno, resid, elety, eleno, chain).}
#' @return \item{start}{ a vector with the first frame of the simulation. When multiple simulations are concatenated with \code{cat.trj} the vector indicates the first frame of each simulation.}
#' @return \item{end}{ a vector with the last frame of the simulation. When multiple simulations are concatenated with \code{cat.trj} the vector indicates the last frame of each simulation.}
#' @return \item{call}{ the matched call.}
#'
#' @export
#'
#' @examples
#' trj <- read.trj(trjfile="rep_001.xtc", topfile="ref.pdb"))

read.trj <- function(trjfile, topfile){
  supported_top_formats <- c("pdb","gro")
  supported_trj_formats <- c("dcd","xtc")

  topfileExtension <- tools::file_ext(topfile)
  trjfileExtension <- tools::file_ext(trjfile)

  topfilepath <- tools::file_path_as_absolute(topfile)
  trjfilepath <- tools::file_path_as_absolute(trjfile)

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
    trj_start <- c(NA)
    trj_end <- c(NA)
  }

  if(trjfileExtension == "xtc"){
    trj_xtc <- rio_read_xtc(trjfile)
    trj_coord <- trj_xtc
    trj_start <- c(1)
    trj_end <- c(dim(trj_xtc)[3])
  }

  trj <- NULL

  trj$topfile <- topfilepath
  trj$topformat <- topfileExtension
  trj$trjfile <- trjfilepath
  trj$trjformat <- trjfileExtension
  trj$coord <- trj_coord
  trj$top <- top
  trj$start <- trj_start
  trj$end<- trj_end
  trj$call <- sys.call()

  class(trj) <- "trj"

  if(nrow(trj$coord) != nrow(trj$top)){
    stop("Inconsistent number of atoms in topology and trajectory.")
  }

  return(trj)
}
