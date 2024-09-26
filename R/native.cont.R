#' @title Select native contact distances
#' @description Function to select only distances between residues making contacts in reference file or a frame of the simulation
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param struct a struct object read with read.struct() to compute the native.cont
#' @param trj a trj object to compute the native.cont
#' @param trj.frame The frame of the trj on which the native.cont are computed
#' @param distance the distance cut-off
#' @param mol.2 can be FALSE (default), use the whole distance matrix, or a vector containing the atomic number of the second molecule (and compute only intermolecular distances)
#' @param atoms can be NULL (default), consider all the atoms present in coords, or a vector containing a set of atomic numbers to consider in the calculation (e.g. only CB). atoms can be obtained with the bio3d atom.select function
#' @return A vector containing the index of a subset of selected distances
#' @export
#' @examples
#' # Read reference structure file with native conformation
#' struct <- read.struct(system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' #Select only Cbeta atoms to perform the analysis
#' sele_atoms <- which(struct$atom$elety=="CB")
#' #Choose only native contacts
#' sele_dists <- native.cont(struct=struct, distance=1.0, atoms=sele_atoms)
#'
native.cont <- function(struct=NULL, trj=NULL, trj.frame=1, distance, mol.2=FALSE, atoms=NULL){
    # If nor a struct object nor a trj is given, print an error message
    if(is.null(struct) & is.null(trj)){
        stop("Please provide a struct or a trj as input")
    }
    # If both a struct and a trj is given, print a warning message
    if(is.null(struct)==FALSE & is.null(trj)==FALSE){
        warning("Both a struct and a trj was provided, using coordinate from the struct file and ignoring trj.")
    }
    #Use the structure file
    if(is.null(struct)==FALSE){
        coord <- cbind(struct$atom$x, struct$atom$y, struct$atom$z)
    }
    # Use the trj file
    if(is.null(struct) & is.null(trj)==FALSE){
        coord <- trj$coord[,,trj.frame]
    }
    #Total number of atoms
    Natm <- nrow(coord)
    #If no atom selection is given, atoms will be a vector containing all the atoms
    if(is.null(atoms)){
        atoms <- c(1:Natm)
    }
    #Compute the distance matrix for the frame.
    dist.mat <- calc.dist.mat(coord[atoms,])
    #Select only lower triangle of matrix
    low.triang <- which(lower.tri(dist.mat)==TRUE)
    if(is.logical(mol.2) == FALSE){
        #Check that the second molecule number are in the range 1:Natm
        if(length(which(mol.2 %in% c(1:Natm) == FALSE)) > 0){
            stop(paste("Atoms of the second molecule:\n", mol.2, "\nare not in the range 1-", Natm, "\n", sep=''))
        }
        #Consider only atoms that are within the atom selection (atoms)
        mol.2_id <- which(atoms %in% mol.2)
        prot <- which(c(1:Natm) %in% mol.2 ==FALSE)
        prot_id <- which(atoms %in% prot)
        dist.mat <- dist.mat[prot_id, mol.2_id]
        sele.dist <- which(dist.mat<distance)
    } else {
        if(mol.2==FALSE){
            sele.dist <- which(dist.mat<distance)
            sele.dist <- sele.dist[sele.dist %in% low.triang]
        }
        if(mol.2==TRUE){
            print("mol.2 should be a vector of the second molecule atom indexes")
            stop()
        }
    }
    return(sele.dist)
}
