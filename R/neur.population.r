#' @title Get Neuron Population
#' @description Function to compute the per-neuron population
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM the SOM object
#' @param start a vector containing the start frames of each replica (usually contained in trj$start if replicas were merged with cat_trj)
#' @param end a vector containing the end frames of each replica (usually contained in trj$end if replicas were merged with cat_trj)
#' @param N The portion of simulation that one want to plot
#' @export
#' @examples
#' neur.population(SOM)
#'
neur.population <- function(SOM, start=1, end=length(SOM$unit.classif), N=1){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check whether replica is an integer number
    if(N > length(start)){
        stop("The value of N exceed the number of replicas indicated by the start vector")
    }
    population <- NULL
    #Compute population for each neuron
    for(neuron in 1:nrow(SOM$grid$pts)){
        population <- c(population, length(which(SOM$unit.classif[start[N]:end[N]]==neuron)))
    }
    return(population)
}
