# Compute the cluster representatives
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM a kohonen SOM object. 
#' @param cluster a vector of clusters assignment for each neuron, as returned for example by hclust.
#'
#' @return representatives a vector of frames representatives of each neuron 
#' @export
#'
#' @examples
#' #TODO Not added yet


cluster.representatives <- function(SOM, clusters){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #Compute cluster centroid
    CENTROID <- sapply(unique(clusters), clust.centroid, SOM$codes[[1]], clusters)
    ReprNeurons <- NULL
    for(i in 1:ncol(CENTROID)){
        ReprNeurons <- c(ReprNeurons, Select_representative(CENTROID, SOM, clusters, i))
    }
    NEUR_REP <- neur.representatives(SOM)
    cl_repr <- NULL
    cl_repr$frames <- NEUR_REP[ReprNeurons]
    cl_repr$neurons <- as.numeric(ReprNeurons)
    names(cl_repr$frames) <- LETTERS[1:length(ReprNeurons)]
    names(cl_repr$neurons) <- LETTERS[1:length(ReprNeurons)]
    return(cl_repr)
}


### These are two function used by the previous function
#Function to compute the weighted mean (by population) of the vectors belonging to each clusters
clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    if(sum(ind)>1){
        POP <- NULL
        for(NEURON in 1:nrow(SOM$grid$pts)){
            POP <- c(POP, length(which(SOM$unit.classif==NEURON)))
        }
         apply(dat[ind,], 2, weighted.mean, w=POP[ind])
    } else {
        dat[ind,]
    }
}

#Function to select the neuron representative of cluster CL
Select_representative <- function(CENTROID, SOM, clusters, CL){
    FR <- which(clusters==CL)
    DistCentroid <- apply(SOM$codes[[1]], 1, COMPUTE_DISTANCE, V2=CENTROID[,CL])
    ReprNeuron <- which(DistCentroid==min(DistCentroid[FR]))
    return(ReprNeuron)
}

COMPUTE_DISTANCE <- function(V1, V2){
    return(as.numeric(dist(rbind(V1, V2))))
}
