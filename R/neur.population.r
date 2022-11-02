#' Get Neuron Population
#'
#' Function to compute the per-neuron population
#'
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param SOM the SOM object
#'
#' @export
#'
#' @examples
#' neur.population(SOM) 

neur.population <- function(SOM){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    population <- NULL
    for(neuron in 1:nrow(SOM$grid$pts)){
        population <- c(population, length(which(SOM$unit.classif==neuron)))
    }
    return(population)
}
