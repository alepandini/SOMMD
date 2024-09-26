#' @title Calculation of Distances
#' @description Compute the pairwise distance matrix of a given set of coordinates, and only retain to some selected distances
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#' @param coord matrix of N atomic coordinates (N rows, 3 columns)
#' @param mol.1_id vector containing the index of the first molecule for intermolecular distances only
#' @param mol.2_id vector containing the index of the second molecule for intermolecular distances only
#' @param sele contains the selection of distances coming from the native_contacts function
#' @return A matrix contaning the selected distances for a frame
#'
calc.dists <- function(coord, mol.1_id=FALSE, mol.2_id=FALSE, sele=FALSE){
    N_atm <- nrow(coord)
    #Check that the mol.2_id selection is within the trj number of atoms:
    if(is.logical(mol.2_id) == FALSE){
        if(is.numeric(mol.2_id)==FALSE){
            stop("mol.2_id must be of type numeric, or FALSE")
        }
        if(max(mol.2_id) > N_atm){
            stop(paste("Atoms of the second molecule:\n", mol.2_id, "\nare not in the range 1-", N_atm, "\n", sep=''))
        }
    } else{
        # Check that mol.2_id is not equal to TRUE
        if(mol.2_id == TRUE){
            stop("mol.2_id should be a vector of atom indexes or FALSE")
        }
    }
    # Check that sele is not equal to TRUE
    if(is.logical(sele) == TRUE){
        if(sele==TRUE){
            stop("sele should be a selection of distances obtained from native_contacts or FALSE")
        }
    }
    #Compute distance matrix for all the atoms
    D <- as.matrix(stats::dist(coord), method='euclidean', upper=TRUE, diag=TRUE)
    #Retain only intermolecular distances
    if(is.logical(mol.2_id) == FALSE){
        D <- D[mol.1_id, mol.2_id]
    }
    #Retain only distances within the selection passed with sele
    if(is.logical(sele) == FALSE){
        D <- c(D[sele])
    } else{
        if(sele==FALSE){
            D <- c(D)
        }
    }
    return(D)
}
