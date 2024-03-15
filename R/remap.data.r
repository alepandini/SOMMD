#' map data to existing SOM
#'
#' Assign new data to a pre-trained SOM
#'
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param SOM a trained SOM
#' @param X a data set with the same number of features of the dataset used to train the SOM
#' @param add whether to append the new data to the ones used to train the SOM
#'
#' @return The SOM with the new data mapped
#' @export
#'
#' @examples
#' # Compute distances on new simulations (the same used for SOM training)
#' DIST2 <- calc_distances(trj2, mol.2=FALSE, sele=sele_dists, atoms=sele_atoms)
#' # Map new data on the existing SOM
#' SOM_new <- remap.data(SOM=SOM, X=DIST2)

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
