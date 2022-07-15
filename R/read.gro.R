#' Function to read gro files
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param file contains the name and the path to the gro file to be read
#'
#' @return gro the gro file.
#' @export
#'
#' @examples
#' TODO Not added yet


read.gro <- function(file){
    if (missing(file)) {
        stop("read.gro: please specify a gro 'file' for reading")
    }
    # Read the file with readLines
    LINES <- readLines(file)
    Natm <- as.integer(LINES[2])
    BOX <- scan(text=LINES[length(LINES)], what="", quiet=TRUE)
    #Check if the gro file contains also column for velocities
    if(length(strsplit(LINES[3], split="")[[1]])<50){
        atoms <- t(sapply(LINES[3:(Natm+2)], substring, first=c(1, 6,  11, 16, 21, 29, 37), c(5, 10, 15, 20, 28, 36, 44), USE.NAMES=FALSE))
        atoms <- cbind(atoms, matrix(NA, ncol=3, nrow=Natm))
    } else{
        atoms <- t(sapply(LINES[3:(Natm+2)], substring, first=c(1, 6,  11, 16, 21, 29, 37, 45, 53, 61), c(5, 10, 15, 20, 28, 36, 44, 52, 60, 68), USE.NAMES=FALSE))
    }
    atoms <- as.data.frame(atoms)
    #
    atoms[,c(1,4,5,6,7,8,9,10)] <- lapply(atoms[,c(1,4,5,6,7,8,9,10)], function(x) as.numeric(as.character(x)))
    atoms[,c(2,3)] <- lapply(atoms[,c(2,3)], function(x) as.character(x))
    atoms[,c(1,4,5,6,7,8,9,10)] <- lapply(atoms[,c(1,4,5,6,7,8,9,10)], function(x) round(x, digits=3))
    colnames(atoms) <- c("resno", "resid", "elety", "eleno", "x", "y", "z", "Vx", "Vy", "Vz")
    gro <- NULL
    gro$atom <- atoms
    gro$xyz  <- matrix(rbind(atoms$x, atoms$y, atoms$z), nrow=1)
    gro$box  <- BOX
    class(gro) <- "gro"
    return(gro)
}                    
                    
                    
