#' Function to plot the silhouette profile for the Nclus cluster of the SOM neurons
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM: the SOM object 
#' @param property: a vector containing the per-neuron property to plot
#' @param scale: a number to scale up or down the size of the drawn circles
#'
#' @export
#'

SOM.add.circles <- function(SOM, property, scale=1){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #Dimension of the SOM
    DIM <- max(SOM$grid$xdim, SOM$grid$ydim)
    MAGNIF <- (40/DIM)*scale
    #Rescale the property
    property <- property/max(property[is.na(property)==FALSE])
    #Add points
    points(SOM$grid$pts[,1], SOM$grid$pts[,2], pch=16, cex=property*MAGNIF*1.2, col="black", xpd=TRUE)
    points(SOM$grid$pts[,1], SOM$grid$pts[,2], pch=16, cex=property*MAGNIF, col="white", xpd=TRUE)
}
