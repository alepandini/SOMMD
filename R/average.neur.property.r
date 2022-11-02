#' Compute average property
#'
#' Function to compute the average value of a property for each neuron.
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param SOM: the SOM object to cluster
#' @param P: the property for each frame of the simulation
#'
#' @return avg.neur.p the per-neuron average of the property.
#' @export
#'
#' @examples
#' #Compute distances between these two atoms in every frame of the simulation
#' Term_dist <- apply(trj$coord[Terminals,,], 3, dist)
#' #Compute average property value for each neuron
#' average.neur.property(SOM, Term_dist)
#'
average.neur.property <- function(SOM, P){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check that the length of the property and the length of the data used to train the SOM are consistent
    if(length(SOM$unit.classif) != length(P)){
        stop(paste("SOM input frames were ", length(SOM$unit.classif), " while length of the property is ", length(P), sep=''))
    }
    avg.neur.p <- NULL
    for(i in 1:nrow(SOM$grid$pts)){
        avg.neur.p <- c(avg.neur.p, mean(P[which(SOM$unit.classif==i)]))
    }
    return(avg.neur.p)
}
