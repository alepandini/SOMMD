#' @title Silhouette score
#' @description Function to compute the silhouette score for the clustering of SOM neurons
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM the SOM object to cluster
#' @param dist_clust the metric for the distance calculation
#' @param clust_method the method for the clustering (passed to the hclust function
#' @param interval the cluster number on which the silhouette score will be computed
#' @return D the set of distances used to train the SOM is computed for all the frames.
#' @export
#' @examples
#' silhouette.score(SOM, clust_method="complete", intervall=seq(2,30))
#'
silhouette.score <- function(SOM, dist_clust="euclidean", clust_method="complete", interval=seq(2,30)){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check whether Nclus is lower than the number of neurons
    if(max(interval) > nrow(SOM$grid$pts) ){
        stop("The upper limit of interval cannot exceed the number of SOM neurons")
    }
    SIL <- NULL
    #For the selected interval of number of clusters
    for(i in interval){
            #Compute the silhouette profile
        sil = silhouette.profile(SOM, Nclus=i, dist_clust=dist_clust, clust_method=clust_method)
        SIL <- c(SIL, mean(sil[,3]))
    }
    sil.score <- cbind(interval, SIL)
    return(sil.score)
}
