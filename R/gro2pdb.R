#' Convert a gro object into a pdb obtect
#'
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param gro contains the gro object to convert
#'
#' @return Returns an object with class "pdb"
#'
#' @export
#'
#' @examples
#' pdb <- gro2pdb(gro)

gro2pdb <- function(gro){
    if (missing(gro)) {
        stop("please specify a gro object to convert")
    }
    if(class(gro)!="gro"){
        stop("gro should be an object with class gro")
    }
    #Create empty object
    pdb <- NULL
    pdb$atom <-  data.frame(matrix(nrow = nrow(gro$atom), ncol = 16))
    colnames(pdb$atom) <- c("type", "eleno", "elety", "alt", "resid", "chain", "resno", "insert", "x", "y", "z", "o", "b", "segid", "elesy", "charge")
    #Define standard pdb residue name for protein and nucleic acid
    std.resname <- c("ALA", "CYS", "ASP", "GLU", "PHE", "GLY", "HIS", "ILE", "LYS", "LEU", "MET", "ASN", "PRO", "GLN", "ARG", "SER", "THR", "VAL", "TRP", "TYR", "DA", "DC", "DG", "DT", "DI")
    pdb$atom$type <- "HETATM"
    pdb$atom$type[which(gro$atom$resid %in% std.resname)] <- "ATOM"
    #Fill in pdb columns from gro file
    pdb$atom$eleno <- gro$atom$eleno
    pdb$atom$elety <- gro$atom$elety
    pdb$atom$resid <- gro$atom$resid
    pdb$atom$resno <- gro$atom$resno
    #Convert the coordinates from nm to Angstrom
    pdb$atom$x <- gro$atom$x*10
    pdb$atom$y <- gro$atom$y*10
    pdb$atom$z <- gro$atom$z*10
    #Set occupancy to 1 and b-factor to 0
    pdb$atom$o <- 1
    pdb$atom$b <- 0
    #Other pdb fields
    pdb$xyz <- gro$xyz
    pdb$calpha <- pdb$atom$type %in% c("ALA", "CYS", "ASP", "GLU", "PHE", "GLY", "HIS", "ILE", "LYS", "LEU", "MET", "ASN", "PRO", "GLN", "ARG", "SER", "THR", "VAL", "TRP", "TYR") & 
                  pdb$atom$elety == "CA"
    pdb$call <- sys.call()       
    #Set the class of the object
    class(pdb) <- "pdb"
    return(pdb)
}                    
                    
                    



