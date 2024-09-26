#' @title calc.distances
#' @description Function to compute distances to be used to train the SOM
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param trj contains the trajectory coordinates (array with three dimensions obtained by rioxdr)
#' @param mol.2 contains the atom indexes of the second molecule in case only intermolecular distances should be computed
#' @param sele contains the selection of distances coming from the native_contacts function
#' @param atoms contains a list of atoms indexes on which the distances will be computed
#' @param cap If a number is given, distances greater than this value are set at the cap value
#' @return A matrix containing the set of distances computed for all the frames.
#' @export
#' @examples
#' # Read reference structure file with native conformation
#' struct <- read.struct(system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' # Read the trajectory
#' trj <- read.trj(trjfile = system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD"),
#'   topfile = system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' # Select only Cbeta atoms to perform the analysis
#' sele_atoms <- which(trj$top$elety=="CB")
#' # Choose only native contacts
#' sele_dists <- native.cont(struct=struct, distance=1.0, atoms=sele_atoms)
#' # Compute distances for SOM training.
#' DIST <- calc.distances(trj, mol.2=FALSE, sele=sele_dists, atoms=sele_atoms)
#'
calc.distances <- function(trj, mol.2=FALSE, sele=FALSE, atoms=NULL, cap=NULL){
    #Check that the trajectory is of class trj:
    if(!methods::is(trj,"trj")){
        stop("The trajectory should be an object with class trj")
    }
    N_atm <- nrow(trj$coord)
    #If no atom selection is given, atoms will be a vector containing all the atoms
    if(is.null(atoms)){
        atoms <- c(1:N_atm)
    }
    if(is.numeric(atoms) == FALSE){
            stop("atoms must be of type numeric, or NULL")
    }
    #Check that the mol.2 selection is within the trj number of atoms:
    if(is.logical(mol.2) == FALSE){
        if(is.numeric(mol.2)==FALSE){
            stop("mol.2 must be of type numeric, or FALSE")
        }
        if(max(mol.2) > N_atm){
            stop(paste("Atoms of the second molecule:\n", mol.2, "\nare not in the range 1-", N_atm, "\n", sep=''))
        }
    } else{
        # Check that mol.2 is not equal to TRUE
        if(mol.2 == TRUE){
            stop("mol.2 should be a vector of atom indexes or FALSE")
        }
    }
    # Check that sele is not equal to TRUE
    if(is.logical(sele) == TRUE){
        if(sele==TRUE){
            stop("sele should be a selection of distances obtained from native_contacts or FALSE")
        }
    }
    if(is.null(cap) == FALSE){
        if(is.numeric(cap) == FALSE){
            stop("cap must be a number or NULL")
        }
    }
    coords <- trj$coord[atoms,,,drop=FALSE]
    #Compute intermolecular distances
    if(is.logical(mol.2) == FALSE){
        mol.2_id <- which(atoms %in% mol.2)
        mol.1 <- which(c(1:N_atm) %in% mol.2 ==FALSE)
        mol.1_id <- which(atoms %in% mol.1)
        D <- apply(coords, 3, calc.dists, mol.1_id=mol.1_id, mol.2_id=mol.2_id, sele=sele)
    } else{
        #Compute all the distance matrix
        D <- apply(coords, 3, calc.dists, sele=sele)
    }
    #Apply capping
    if(is.null(cap) == FALSE){
        D[which(D>cap)] <- cap
    }
    return(t(D))
}
