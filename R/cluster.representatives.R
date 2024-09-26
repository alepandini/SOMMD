#' @title Cluster Representatives
#' @description Compute the cluster representatives
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM a kohonen SOM object.
#' @param clusters a vector of clusters assignment for each neuron, as returned for example by hclust.
#' @return A vector of frames representatives of each neuron
#' @export
#' @examples
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' # Divide the SOM in the selected number of clusters
#' som_cl <- cutree(hclust(dist(som_model$codes[[1]], method="euclidean"), method="complete"), 4)
#' #Get representative frames for each cluster
#' cl_repres <- cluster.representatives(som_model, som_cl)
#'
cluster.representatives <- function(SOM, clusters){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check whether the number of elements in clusters vector is equal to neuron numbers
    if(length(clusters) != nrow(SOM$grid$pts)){
        stop("Number of cluster elements is different from number of neurons")
    }
    #Compute cluster centroid
    centroid <- sapply(unique(clusters), clust.centroid, SOM, clusters)
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

#' @title Centroid of a cluster
#' @description Function to compute the weighted mean (by population) of the vectors belonging to each clusters
#' @param i the selected cluster
#' @param SOM a kohonen SOM object
#' @param clusters a vector of clusters assignment for each neuron, as returned for example by hclust
#' @return A vector containing the centroid of a selection of neurons
#' @noRd
#'
clust.centroid <- function(i, SOM, clusters) {
    ind <- (clusters == i)
    if(sum(ind)>1){
        pop <- NULL
        for(neur in 1:length(clusters)){
            pop <- c(pop, length(which(SOM$unit.classif==neur)))
        }
         return(apply(SOM$codes[[1]][ind,], 2, stats::weighted.mean, w=pop[ind]))
    } else {
        return(SOM$codes[[1]][ind,])
    }
}

#' @title Select representative neuron
#' @description Function to select the neuron representative of cluster cl
#' @param centroid a matrix containing the centroids of all the clusters by column (computed with clust.centroid)
#' @param SOM the SOM object
#' @param clusters a vector of clusters assignment for each neuron, as returned for example by hclust
#' @param cl the cluster for which the representative neuron should be computed
#' @return An integere with the representative neuron number for the cluster
#' @noRd
#'
select_representative <- function(centroid, SOM, clusters, cl){
    frame <- which(clusters==cl)
    dist.centroid <- apply(SOM$codes[[1]], 1, compute.distance, V2=centroid[,cl])
    repr.neur <- which(dist.centroid==min(dist.centroid[frame]))
    return(repr.neur)
}
