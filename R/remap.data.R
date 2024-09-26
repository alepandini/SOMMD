#' @title map data to existing SOM
#' @description Assign new data to a pre-trained SOM
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM a trained SOM
#' @param X a data set with the same number of features of the dataset used to train the SOM
#' @param add whether to append the new data to the ones used to train the SOM
#' @return An object of class "kohonen" with the new data mapped
#' @export
#' @examples
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #Read a trajectory that was not used to train the som
#' trj_2 <- read.trj(trjfile = system.file("extdata", "HIF2a-MD-2.xtc", package = "SOMMD"),
#'   topfile = system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' #Read reference structure file
#' gro <- read.struct(system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' #Selection of the same intermolecular distances used to train the SOM
#' protein.sele <- which(gro$atom$resid!="020")
#' ligand.sele <- which(gro$atom$resid=="020")
#' heavy.atoms <- which(startsWith(gro$atom$elety, "H")==FALSE)
#' sele.dists <- native.cont(struct=gro, distance=0.6, mol.2=ligand.sele, atoms=heavy.atoms)
#' # Compute distances on new simulations (the same used for SOM training)
#' dist_2 <- calc.distances(trj_2, mol.2=ligand.sele, sele=sele.dists, atoms=heavy.atoms)
#' # Map new data on the existing SOM
#' som_model_2 <- remap.data(SOM=som_model, X=dist_2)
remap.data <- function(SOM, X, add=FALSE)    {
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #Check the the X input data have the same number of features of the trained SOM
    if(ncol(SOM$data[[1]])!=ncol(X)){
        stop(paste("The number of features used to train the SOM (", ncol(SOM$data[[1]]), ") is different from the number of features of X (", ncol(X), ").", sep=''))
    }
    if(add != FALSE & add != TRUE){
        stop("add must be set to the value of TRUE or FALSE")
    }
    SOM_new <- SOM
    # The new SOM will contain only new data
    if(add==FALSE){
        SOM_new$data[[1]] <- X
        MAP <- kohonen::map(x=SOM, newdata=X)
        SOM_new$unit.classif <- MAP$unit.classif
        SOM_new$distances <- MAP$distances
    # The new SOM will contain both the old and the new data
    } else{
        SOM_new$data[[1]] <- rbind(SOM$data[[1]], X)
        MAP <- kohonen::map(x=SOM, newdata=X)
        SOM_new$unit.classif <- c(SOM$unit.classif, MAP$unit.classif)
        SOM_new$distances <- c(SOM$distances, MAP$distances)
    }
    return(SOM_new)
}
