#' @title Read structure files
#' @description Function to read pdb and gro files
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param file contains the name and the path to the pdb or gro file to be read
#' @return Returns a list of class "struct" with the following components:
#' @return \item{atom}{ a data frame containing all atomic coordinate with a row per atom and a column per record type.}
#' @return \item{xyz}{ a numeric matrix of class "xyz" containing the atomic coordinate data.}
#' @return \item{box}{ a vector of box size.}
#' @return \item{format}{ The format of the original file }
#' @return \item{call}{ the matched call.}
#' @export
#' @examples
#' # Read structure file
#' struct <- read.struct(system.file("extdata", "HIF2a.gro", package = "SOMMD"))
read.struct <- function(file){
  supported_formats <- c("pdb","gro")
  fileExtension <- tools::file_ext(file)
  filepath <- tools::file_path_as_absolute(file)

  if(!fileExtension %in% supported_formats){
    stop("SOMMD currely does not support this topology format.")
  }

  if(fileExtension == "pdb"){
    top_pdb <- bio3d::read.pdb(file, verbose = F)
    #Add properties missing in PDB format
    top_pdb$atom$Vx <- NA
    top_pdb$atom$Vy <- NA
    top_pdb$atom$Vz <- NA
    pdb_columns <- c("eleno", "elety", "resid", "chain", "resno", "x", "y", "z", "Vx", "Vy", "Vz", "o", "b")
    top <- top_pdb$atom[,pdb_columns]
    #Convert Angstrom to nm
    top$x <- top$x/10
    top$y <- top$y/10
    top$z <- top$z/10
    box <- c(0.0, 0.0, 0.0)
    format <- "pdb"
  }

  if(fileExtension == "gro"){
    top_gro <- read.gro(file)
    #Add properties missing in GRO format
    top_gro$atom$chain <- NA
    top_gro$atom$o <- NA
    top_gro$atom$b <- NA
    gro_columns <- c("eleno", "elety", "resid", "chain", "resno", "x", "y", "z", "Vx", "Vy", "Vz", "o", "b")
    box <- top_gro$box
    top <- top_gro$atom[,gro_columns]
    format <- "gro"
  }
  #Create the struct object
  struct <- NULL
  struct$atom <- top
  struct$atom$resid <- sapply(struct$atom$resid, trimws)
  struct$atom$elety <- sapply(struct$atom$elety, trimws)
  struct$xyz  <- matrix(rbind(struct$atom$x, struct$atom$y, struct$atom$z), nrow=1)
  struct$box  <- box
  struct$format <- format
  struct$call <- sys.call()
  class(struct) <- "struct"
  return(struct)
}
