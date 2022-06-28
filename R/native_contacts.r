#' Function to select only distances between residues making contacts in reference file or a frame of the simulation
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param coord matrix of N atomic coordinates (N rows, 3 columns) that will be used to determine the native_contacts
#' @param distance the distance cut-off
#' @param LIG can be FALSE (default), use the whole distance matrix, or a vector containing the ligand atomic number (and compute only intermolecular disatnces)
#' @param atoms can be FALSE (default), consider all the atoms present in coords, or a vector containing a set of atomic numbers to consider in the calculation (e.g. only CB). atoms can be obtained with the bio3d atom.select function
#'
#' @return SELE the selection of distances
#' @export
#'
#' @examples
#' TODO Not added yet



native_contacts <- function(coord, distance, LIG=FALSE, atoms=NULL){
    #If the coord are a pdb file, convert it to a xyz matrix
    if(is.pdb(coord)){
        #get the coordinate from pdb and convert them from Angstrom to nm
        coord <- t(matrix(pdb$xyz, nrow=3))/10
    }
    #If a trajectory is given as input, automatically use the first frame of the trj
    if(length(dim(coord)) > 2){
        coord <- coord[,,1]
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
    if(is.logical(LIG) == FALSE){
        #Check that the ligand number are in the range 1:Natm
        if(length(which(LIG %in% c(1:N_atm) == FALSE)) > 0){
            cat(paste("Ligands atoms:\n", LIG, "\nare not in the range 1-", N_atm, "\n", sep=''))
            return(NULL)
        }
        #Consider only ligand and protein atoms that are within the atom selection (atoms)
        LIG_id <- which(atoms %in% LIG)
        PROT <- which(c(1:N_atm) %in% LIG ==FALSE)
        PROT_id <- which(atoms %in% PROT)
        DIST_MAT <- DIST_MAT[PROT_id, LIG_id]
        SELE <- which(DIST_MAT<distance)
    } else {
        if(LIG==FALSE){
            SELE <- which(DIST_MAT<distance)
            SELE <- SELE[SELE %in% LOWTRI]
        }
        if(LIG==TRUE){
            print("LIG should be a vector of ligand atom indexes")
            stop()
        }
    }
    return(SELE)
}
