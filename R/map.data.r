#' Assign new data to a trained SOM
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM a trained SOM
#' @param X a data set with the same number of features of the dataset used to train the SOM
#'
#' @return MAT
#' @export
#'

#Function to compute distance matrix given a set of coordinate (matrix)
map.data <- function(SOM, X, add=FALSE)    {
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #Check the the X input data have the same number of features of the trained SOM
    if(ncol(SOM$data[[1]])!=ncol(X)){
        stop(paste("The number of features used to train the SOM (", ncol(SOM$data[[1]]), ") is different from the number of features of X (", ncol(X), ").", sep=''))
    }
    if(add!=FALSE & add!=TRUE){
        stop("add must be set to the value of TRUE or FALSE")
    }
    SOM_new <- SOM
    if(add==FALSE){
        SOM_new$data[[1]] <- X
        MAP <- kohonen::map(x=SOM, newdata=X)
        SOM_new$unit.classif <- MAP$unit.classif
        SOM_new$distances <- MAP$distances
    } else{
        SOM_new$data[[1]] <- rbind(SOM$data[[1]], X)
        MAP <- kohonen::map(x=SOM, newdata=X)
        SOM_new$unit.classif <- c(SOM$unit.classif, MAP$unit.classif)
        SOM_new$distances <- c(SOM$distances, MAP$distances)
    }
    return(SOM_new)
}

### Additional function necessary to run previous function

# CALCDIST <- function(V1, V2){
#     return(sqrt(sum((V1-V2)**2)))
# }
