#' @title Convert structure to pdb object
#' @description Convert a struct object into a pdb obtect
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param struct contains the struct object to convert
#' @return Returns an object with class "pdb"
#' @return An object of class "pdb"
#' @export
#' @examples
#' # Read structure file 
#' struct <- read.struct(system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' #Convert structure to pdb object
#' pdb <- struct2pdb(struct)
#'
struct2pdb <- function(struct){
    if (missing(struct)) {
        stop("please specify a struct object to convert")
    }
    if(!methods::is(struct,"struct")){
        stop("struct should be an object with class struct")
    }
    #Create empty object
    pdb <- NULL
    pdb$atom <-  data.frame(matrix(nrow = nrow(struct$atom), ncol = 16))
    colnames(pdb$atom) <- c("type", "eleno", "elety", "alt", "resid", "chain", "resno", "insert", "x", "y", "z", "o", "b", "segid", "elesy", "charge")
    #Define standard pdb residue name for protein and nucleic acid
    std.resname <- c("ALA", "CYS", "ASP", "GLU", "PHE", "GLY", "HIS", "ILE", "LYS", "LEU", "MET", "ASN", "PRO", "GLN", "ARG", "SER", "THR", "VAL", "TRP", "TYR", "DA", "DC", "DG", "DT", "DI")
    pdb$atom$type <- "HETATM"
    pdb$atom$type[which(struct$atom$resid %in% std.resname)] <- "ATOM"
    #Fill in pdb columns from struct file
    pdb$atom$eleno <- struct$atom$eleno
    pdb$atom$elety <- struct$atom$elety
    pdb$atom$resid <- struct$atom$resid
    pdb$atom$resno <- struct$atom$resno
    #Convert the coordinates from nm to Angstrom
    pdb$atom$x <- struct$atom$x*10
    pdb$atom$y <- struct$atom$y*10
    pdb$atom$z <- struct$atom$z*10
    #Set occupancy to 1 and b-factor to 0
    pdb$atom$o <- 1
    pdb$atom$b <- 0
    #Other pdb fields
    pdb$xyz <- struct$xyz
    pdb$calpha <- pdb$atom$type %in% c("ALA", "CYS", "ASP", "GLU", "PHE", "GLY", "HIS", "ILE", "LYS", "LEU", "MET", "ASN", "PRO", "GLN", "ARG", "SER", "THR", "VAL", "TRP", "TYR") &
                  pdb$atom$elety == "CA"
    pdb$call <- sys.call()
    #Set the class of the object
    class(pdb) <- "pdb"
    return(pdb)
}
