#' Cluster Representatives
#' 
#' Compute the cluster representatives
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param SOM a kohonen SOM object. 
#' @param cluster a vector of clusters assignment for each neuron, as returned for example by hclust.
#'
#' @return representatives a vector of frames representatives of each neuron 
#' @export
#'
#' @examples
#' # Divide the SOM in the selected number of clusters
# SOM.hc <- cutree(hclust(dist(SOM$codes[[1]], method="euclidean"), method="complete"), 8)
#' #Get representative frames for each cluster
#' CL_repres <- cluster.representatives(SOM, SOM.hc)
cluster.representatives <- function(SOM, clusters){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check whether the number of elements in clusters vector is equal to neuron numbers
    if(length(SOM.hc) != nrow(SOM$grid$pts)){
        stop("Number of cluster elements is different from number of neurons")
    }
    #Compute cluster centroid
    centroid <- sapply(unique(clusters), clust.centroid, SOM$codes[[1]], clusters)
    repr.neur <- NULL
    #Compute the conformation closer to the centroid and store them in cl_repr
    for(i in 1:ncol(centroid)){
        repr.neur <- c(repr.neur, select_representative(centroid, SOM, clusters, i))
    }
    neur.representatives <- neur.representatives(SOM)
    cl_repr <- NULL
    cl_repr$frames <- neur.representatives[repr.neur]
    cl_repr$neurons <- as.numeric(repr.neur)
    names(cl_repr$frames) <- LETTERS[1:length(repr.neur)]
    names(cl_repr$neurons) <- LETTERS[1:length(repr.neur)]
    return(cl_repr)
}


### Internal Function
#' Centroid of a cluster
#'
#' Function to compute the weighted mean (by population) of the vectors belonging to each clusters
#' @param i the selected cluster
#' @param dat the codebook vector of each cluster
#' @param clusters a vector of clusters assignment for each neuron, as returned for example by hclust  
#'
#' @return the centroid of a selection of neurons
#' @noRd
#'
#' @examples
#' # Perform clustering of neurons
#' clusters <- cutree(hclust(dist(SOM$codes[[1]], method="euclidean"), method="complete"), 8)
#' # Compute cluster centroid for all the clusters
#' centroid <- sapply(unique(clusters), clust.centroid, SOM$codes[[1]], clusters)
clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    if(sum(ind)>1){
        pop <- NULL
        for(neur in 1:nrow(SOM$grid$pts)){
            pop <- c(pop, length(which(SOM$unit.classif==neur)))
        }
         return(apply(dat[ind,], 2, stats::weighted.mean, w=pop[ind]))
    } else {
        return(dat[ind,])
    }
}

### Internal Function
#' Select representative neuron
#'
#' Function to select the neuron representative of cluster cl
#' @param centroid a matrix containing the centroids of all the clusters by column (computed with clust.centroid)
#' @param SOM the SOM object
#' @param clusters a vector of clusters assignment for each neuron, as returned for example by hclust
#' @param cl the cluster for which the representative neuron should be computed
#'
#' @return the representative neuron
#' @noRd
#'
#' @examples
#' # Perform clustering of neurons
#' clusters <- cutree(hclust(dist(SOM$codes[[1]], method="euclidean"), method="complete"), 8)
#' # Compute cluster centroid
#' centroid <- sapply(unique(clusters), clust.centroid, SOM$codes[[1]], clusters)
#' #Select the representative neuron of the first cluster
#' repr.neur <- select_representative(centroid, SOM, clusters, 1))
select_representative <- function(centroid, SOM, clusters, cl){
    frame <- which(clusters==cl)
    dist.centroid <- apply(SOM$codes[[1]], 1, compute.distance, V2=centroid[,cl])
    repr.neur <- which(dist.centroid==min(dist.centroid[frame]))
    return(repr.neur)
}


