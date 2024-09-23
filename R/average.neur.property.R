#' @title Compute average property
#' @description Function to compute the average value of a property for each neuron.
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM the SOM object to cluster
#' @param P the property for each frame of the simulation
#' @return The a vector with the per-neuron average of the property.
#' @export
#' @examples
#' #Read trajectory
#' trj <- read.trj(trjfile = system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD"),
#'   topfile = system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #Compute distance between two atoms in every frame of the simulation
#' Distance <- apply(trj$coord[c(162,1794),,], 3, dist)
#' #Compute average property value for each neuron
#' avg.p <- average.neur.property(som_model, Distance)
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
    #For every neuron, compute the average value of the proporty for the frames of that neuron
    for(i in 1:nrow(SOM$grid$pts)){
        avg.neur.p <- c(avg.neur.p, mean(P[which(SOM$unit.classif==i)]))
    }
    return(avg.neur.p)
}
