#' Function to compute distances to be used to train the SOM
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param trj contains the trajectory coordinates (array with three dimensions obtained by rioxdr)
#' @param skip can skip frames (used to stride the trajectory)
#' @param LIG contains the ligand atom indexes in case only intermolecular distances should be computed
#' @param sele contains the selection of distances coming from the native_contacts function
#'
#' @return D the set of distances used to train the SOM is computed for all the frames.
#' @export
#'
#' @examples
#' TODO Not added yet


calc_distances <- function(trj, skip=1, LIG=FALSE, sele=FALSE, atoms=NULL){
    N_atm <- nrow(trj)
    #If no atom selection is given, atoms will be a vector containing all the atoms
    if(is.null(atoms)){
        atoms <- c(1:N_atm)
    }
    #Compute distance matrix for all the frames
    D <- apply(trj[atoms,,seq(1, dim(trj)[3], by=skip)], 3, Calc_Dist_Mat)
    if(is.logical(LIG) == FALSE){
        #Check that the ligand number are in the range 1:Natm
        if(length(which(LIG %in% c(1:N_atm) == FALSE)) > 0){
            cat(paste("Ligands atoms:\n", LIG, "\nare not in the range 1-", N_atm, "\n", sep=''))
            return(NULL)
        }
        #Consider only ligand and protein atoms that are within the atom selection (atoms)
        #Convert the 2D matrix of distances in 3D matrix
        DIST_MAT <- array(D, dim=c(c(1:length(atoms)),c(1:length(atoms)),ncol(D)))
        LIG_id <- which(atoms %in% LIG)
        PROT <- which(c(1:N_atm) %in% LIG ==FALSE)
        PROT_id <- which(atoms %in% PROT)
        D <- DIST_MAT[PROT_id, LIG_id,]
        D <- array(D, dim=c(dim(D)[1]*dim(D)[2], dim(D)[3]))
    }
    if(is.logical(sele) == FALSE){
        return(D[sele,])
    } else{
        if(sele==FALSE){
            return(D)
        }
        if(sele==TRUE){
            print("sele should be a selection of distances obtained from native_contacts")
        stop()
        }
    }
}
