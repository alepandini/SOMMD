# Compute the neurons representatives
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM a kohonen SOM object. 
#'
#' @return representatives a vector of frames representatives of each neuron 
#' @export
#'
#' @examples
#' #TODO Not added yet

neur.representatives <- function(SOM){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #Compute representatives
    representatives <- NULL
    for(neuron in 1:nrow(SOM$grid$pts)){
        SET <- which(SOM$unit.classif==neuron)
        if(length(SET)==0){
            representatives <- c(representatives, NA)
        } else{
            representatives <- c(representatives, SET[which(SOM$distances[SET]==min(SOM$distances[SET]))[1]])
        }
    }
    return(representatives)
}
