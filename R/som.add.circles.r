#' Add cicles to SOM 
#'
#' Function to add circles to a SOM plot, with dimension proportional to a selected property
#'
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param SOM: the SOM object 
#' @param property: a vector containing the per-neuron property to plot
#' @param scale: a number to scale up or down the size of the drawn circles
#' @param col.circles the background color of the drawn circles 
#'
#' @export
#' @examples
#' # Compute per neuron population
#' population <- NULL
#' for(NEURON in 1:nrow(SOM$grid$pts)){
#'     population <- c(population, length(which(SOM$unit.classif==NEURON)))
#' }
#' # Add circles to the SOM plot
#' SOM.add.circles(SOM, population, scale=0.9)

SOM.add.circles <- function(SOM, property, scale=1, col.circles="white"){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    if(length(property) != nrow(SOM$grid$pts)){
        stop(paste("The element of the property vector (", length(property) ,") is different from the number of neurons (", nrow(SOM$grid$pts), ")", sep=''))
    }
    
    #Dimension of the SOM
    DIM <- max(SOM$grid$xdim, SOM$grid$ydim)
    MAGNIF <- (40/DIM)*scale
    #Rescale the property
    property <- property/max(property[is.na(property)==FALSE])
    #Add points
    points(SOM$grid$pts[,1], SOM$grid$pts[,2], pch=16, cex=property*MAGNIF*1.2, col="black", xpd=TRUE)
    points(SOM$grid$pts[,1], SOM$grid$pts[,2], pch=16, cex=property*MAGNIF, col=col.circles, xpd=TRUE)
}
