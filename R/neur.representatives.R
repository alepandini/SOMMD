#' @title Neuron representative
#' @description Compute the representative frame of each neuron (the closest to the neuron codebook)
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM a kohonen SOM object.
#' @return A vector containing the index of the representative frames for each neuron
#' @export
#' @examples
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #Compute representative frame for each neuron
#' neuron_representatives <- neur.representatives(som_model)
neur.representatives <- function(SOM){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #Compute representatives
    representatives <- NULL
    for(neuron in 1:nrow(SOM$grid$pts)){
        #Compute the set of frames belonging to that neuron
        SET <- which(SOM$unit.classif==neuron)
        if(length(SET)==0){
            representatives <- c(representatives, NA)
        } else{
            #The representative is the closer to the neuron codes
            representatives <- c(representatives, SET[which(SOM$distances[SET]==min(SOM$distances[SET]))[1]])
        }
    }
    return(representatives)
}
