#' Function to select only distances between residues making contacts in reference file or a frame of the simulation
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param structure: a pdb or gro object to compute the native_contacts
#' @param trj: a trj object to compute the native_contacts
#' @param trj_frame: The frame of the trj on which the native_contacts are computed
#' @param distance the distance cut-off
#' @param MOL2 can be FALSE (default), use the whole distance matrix, or a vector containing the atomic number of the second molecule (and compute only intermolecular distances)
#' @param atoms can be FALSE (default), consider all the atoms present in coords, or a vector containing a set of atomic numbers to consider in the calculation (e.g. only CB). atoms can be obtained with the bio3d atom.select function
#'
#' @return SELE the selection of distances
#' @export
#'
#' @examples




native_contacts <- function(struct=NULL, trj=NULL, trj_frame=1, distance, MOL2=FALSE, atoms=NULL){
    # If nor a structure file (pdb or gro) nor a trj is given, print an error message
    if(is.null(struct) & is.null(trj)){
        stop("Please provide a structure (pdb or gro) or a trj as input")
    }
    # If both a structure file (pdb or gro) and a trj is given, print a warning message
    if(is.null(struct)==FALSE & is.null(trj)==FALSE){
        warning("Both a structure and a trj was provided, using coordinate from the structure file and ignoring trj.")
    }
    #Use the structure file
    if(is.null(struct)==FALSE){
        coord <- cbind(struct$atom$x, struct$atom$y, struct$atom$z)
        #If it is a pdb file, convert Angstrom to nm
        if("pdb" %in% class(pdb)){
            coord <- coord/10
        }
    }
    # Use the trj file
    if(is.null(struct) & is.null(trj)==FALSE){
        coord <- trj$coord[,,trj_frame]
    }
    #Total number of atoms
    N_atm <- nrow(coord)
    #If no atom selection is given, atoms will be a vector containing all the atoms
    if(is.null(atoms)){
        atoms <- c(1:N_atm)
    }
    #Compute the distance matrix for the frame.
    DIST_MAT <- Calc_Dist_Mat(coord[atoms,])
    #Select only lower triangle of matrix
    LOWTRI <- which(lower.tri(DIST_MAT)==TRUE)
    if(is.logical(MOL2) == FALSE){
        #Check that the second molecule number are in the range 1:Natm
        if(length(which(MOL2 %in% c(1:N_atm) == FALSE)) > 0){
            cat(paste("Atoms of the second molecule:\n", MOL2, "\nare not in the range 1-", N_atm, "\n", sep=''))
            return(NULL)
        }
        #Consider only atoms that are within the atom selection (atoms)
        MOL2_id <- which(atoms %in% MOL2)
        PROT <- which(c(1:N_atm) %in% MOL2 ==FALSE)
        PROT_id <- which(atoms %in% PROT)
        DIST_MAT <- DIST_MAT[PROT_id, MOL2_id]
        SELE <- which(DIST_MAT<distance)
    } else {
        if(MOL2==FALSE){
            SELE <- which(DIST_MAT<distance)
            SELE <- SELE[SELE %in% LOWTRI]
        }
        if(MOL2==TRUE){
            print("MOL2 should be a vector of the second molecule atom indexes")
            stop()
        }
    }
    return(SELE)
}
