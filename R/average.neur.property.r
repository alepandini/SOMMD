#' Function to plot the silhouette score for the clustering of SOM neurons
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM: the SOM object to cluster
#' @param P: the property for each frame of the simulation
#'
#' @return D the set of distances used to train the SOM is computed for all the frames.
#' @export
#'

#Function to compute and plot the silhouette profiles
average.neur.property <- function(SOM, P){
    #check that the length of the property and the length of the data used to train the SOM are consistent
    if(length(SOM$unit.classif) != length(P)){
        stop(paste("SOM input frames were ", length(SOM$unit.classif), " while length of the property is ", length(P), sep=''))
    }
    AVG_NEUR_P <- NULL
    for(i in 1:nrow(SOM$grid$pts)){
        AVG_NEUR_P <- c(AVG_NEUR_P, mean(P[which(SOM$unit.classif==i)]))
    }
    return(AVG_NEUR_P)
}
