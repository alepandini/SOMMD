#' @title Silhouette score
#' @description Function to compute the silhouette score for the clustering of SOM neurons
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM the SOM object to cluster
#' @param dist_clust the metric for the distance calculation
#' @param clust_method the method for the clustering (passed to the hclust function
#' @param interval the cluster number on which the silhouette score will be computed
#' @return A vector with the silhouette scores for all the frames
#' @export
#' @examples
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #Compute the silhouette profile
#' sil_score <- silhouette.score(som_model, clust_method="complete", interval=seq(2,8))
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
