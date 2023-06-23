#' Silhouette profile
#'
#' Function to compute the silhouette profile for the Nclus cluster of the SOM neurons
#'
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param SOM the SOM object to cluster
#' @param Nclus the cluster number on which the silhouette profile will be computed
#' @param dist_clust the metric for the distance calculation
#' @param clust_method the method for the clustering (passed to the hclust function
#'
#' @return the silhouette profile computed with the cluster package
#' @export
#'
#' @examples
#' silhouette.profile(SOM, Nclus=8, clust_method="complete")

silhouette.profile <- function(SOM, Nclus, dist_clust="euclidean", clust_method="complete"){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check that Nclus is lower than the number of neurons
    if(Nclus > nrow(SOM$grid$pts) ){
        stop("The number of cluster cannot exceed the number of SOM neurons")
    }
    #Do clustering
    SOM.hc <- cutree(hclust(dist(SOM$codes[[1]], method=dist_clust), method=clust_method), Nclus)
    #Perform silhouette profile trhough "cluster" library
    sil = cluster::silhouette(SOM.hc, dist(SOM$codes[[1]]))
    return(sil)
}
