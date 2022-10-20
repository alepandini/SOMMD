#' Write an ndx file containing frames belonging to each cluster 
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM a kohonen SOM object. 
#' @param filename the name of the ndx file that will be written out. 
#' @param SOM.hc A vector containing the cluster to which the neurons belong 
#' @param clusters the cluster that should be extracted to the ndx
#' @param append whether to append the group to an existing ndx file, or to overwrite it (if exist)
#'
#' @return 
#' @export
#'
#' @examples
#' #TODO Not added yet

clusters_to_ndx <- function(SOM, filename, SOM.hc, cluster="all", append=FALSE){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check that SOM.hc is a vector with the size of the map
    if(is.vector(SOM.hc)==FALSE){
        stop("SOM.hc must be a vector containing the cluster to which the neurons belong")
    }
    if(length(SOM.hc) != nrow(SOM$grid$pts)){
        stop("SOM.hc must have the same length as the number of neurons")
    }
    #Remove the ndx file if exist and append=false
    if(append==FALSE & file.exists(filename)){
        file.remove(filename)
    }
    if(cluster=="all"){
        cluster <- sort(unique(SOM.hc))
    }
    for(i in cluster){
        NEUR <- which(SOM.hc==i)
        SEQ <- NULL
        for(j in NEUR){
            SEQ <- c(SEQ, which(SOM$unit.classif==j))
        }
        if(length(SEQ>0)){
            SEQ <- sort(SEQ)
            frames_to_ndx(filename=filename, group=paste("Cluster_", i, sep=''), frames=SEQ, append=TRUE)
        }
    }
}
