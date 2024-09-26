#' @title Read gro file
#' @description Function to read gro files
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param file contains the name and the path to the gro file to be read
#' @return Returns a list of class "gro" with the following components:
#' @return \item{atom}{ a data frame containing all atomic coordinate with a row per atom and a column per record type.}
#' @return \item{xyz}{ a numeric matrix of class "xyz" containing the atomic coordinate data.}
#' @return \item{box}{ a vector of box size.}
#' @return \item{call}{ the matched call.}
read.gro <- function(file){
    if (missing(file)) {
        stop("please specify a gro 'file' for reading")
    }
    if (file.exists(file)==FALSE) {
        stop("file not found")
    }
    # Read the file with readLines
    lines <- readLines(file)
    Natm <- as.integer(lines[2])
    box <- scan(text=lines[length(lines)], what="", quiet=TRUE)
    #Check if the gro file contains also column for velocities
    if(length(strsplit(lines[3], split="")[[1]])<50){
        atoms <- t(sapply(lines[3:(Natm+2)], substring, first=c(1, 6,  11, 16, 21, 29, 37), c(5, 10, 15, 20, 28, 36, 44), USE.NAMES=FALSE))
        atoms <- cbind(atoms, matrix(NA, ncol=3, nrow=Natm))
    } else{
        atoms <- t(sapply(lines[3:(Natm+2)], substring, first=c(1, 6,  11, 16, 21, 29, 37, 45, 53, 61), c(5, 10, 15, 20, 28, 36, 44, 52, 60, 68), USE.NAMES=FALSE))
    }
    atoms <- as.data.frame(atoms)
    #create the dataframe with informations about atoms in the gro file
    atoms[,c(1,4,5,6,7,8,9,10)] <- lapply(atoms[,c(1,4,5,6,7,8,9,10)], function(x) as.numeric(as.character(x)))
    atoms[,c(2,3)] <- lapply(atoms[,c(2,3)], function(x) as.character(x))
    atoms[,c(1,4,5,6,7,8,9,10)] <- lapply(atoms[,c(1,4,5,6,7,8,9,10)], function(x) round(x, digits=3))
    #Set the column names compatible with bio3d pdb object
    colnames(atoms) <- c("resno", "resid", "elety", "eleno", "x", "y", "z", "Vx", "Vy", "Vz")
    gro <- NULL
    #Create the gro object
    gro$atom <- atoms
    gro$atom$resid <- sapply(gro$atom$resid, trimws)
    gro$atom$elety <- sapply(gro$atom$elety, trimws)
    gro$xyz  <- matrix(rbind(atoms$x, atoms$y, atoms$z), nrow=1)
    gro$box  <- as.numeric(box)
    gro$call <- sys.call()
    class(gro) <- "gro"
    return(gro)
}
