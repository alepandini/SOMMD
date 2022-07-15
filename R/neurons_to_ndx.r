#Write an ndx file containing frames belonging to each neuron 
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM a kohonen SOM object. 
#' @param filename the name of the ndx file that will be written out. 
#' @param neuron the neurons that should be extracted to the ndx
#' @param append whether to append the group to an existing ndx file, or to overwrite it (if exist)
#'
#' @return 
#' @export
#'
#' @examples
#' #TODO Not added yet

neurons_to_ndx <- function(SOM, filename, neuron="all", append=FALSE){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #Remove the ndx file if exist and append=false
    if(append==FALSE & file.exists(filename)){
        file.remove(filename)
    }
    
    if(neuron=="all"){
        neuron <- c(1:nrow(SOM$grid$pts))
    }
    for(i in neuron){
        SEQ <- which(SOM$unit.classif==i)
        if(length(SEQ)>0){
            frames_to_ndx(filename=filename, group=paste("Neuron_", i, sep=''), frames=SEQ, append=TRUE)
        }
    }
}
