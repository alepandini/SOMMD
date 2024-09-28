#' @title Add circles to SOM
#' @description Function to add circles to a SOM plot, with dimension proportional to a selected property
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM the SOM object
#' @param P a vector containing the per-neuron property to plot
#' @param scale a number to scale up or down the size of the drawn circles
#' @param col.circles the background color of the drawn circles
#' @return Called for its effect.
#' @export
#' @examples
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' # Compute per neuron population
#' pop <- neur.population(som_model)
#' #Plot the som
#' plot(som_model, type = "count", bgcol=c("red", "blue", "yellow", "green"), shape='straight')
#' # Add circles to the SOM plot
#' som.add.circles(som_model, pop, scale=0.9)
#'
som.add.circles <- function(SOM, P, scale=1, col.circles="white"){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    if(length(P) != nrow(SOM$grid$pts)){
        stop(paste("The element of the property vector (", length(P) ,") is different from the number of neurons (", nrow(SOM$grid$pts), ")", sep=''))
    }

    #Dimension of the SOM
    DIM <- max(SOM$grid$xdim, SOM$grid$ydim)
    MAGNIF <- (40/DIM)*scale
    #Rescale the property
    P <- P/max(P[is.na(P)==FALSE])
    #Add points
    graphics::points(SOM$grid$pts[,1], SOM$grid$pts[,2], pch=16, cex=P*MAGNIF*1.2, col="black", xpd=TRUE)
    graphics::points(SOM$grid$pts[,1], SOM$grid$pts[,2], pch=16, cex=P*MAGNIF, col=col.circles, xpd=TRUE)
}
