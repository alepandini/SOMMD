#' Function to plot the silhouette profile for the Nclus cluster of the SOM neurons
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM the SOM object to cluster
#' @param Nclus the cluster number on which the silhouette profile will be computed
#' @param dist_clust the metric for the distance calculation
#' @param clust_method the method for the clustering (passed to the hclust function
#'
#' @return D the set of distances used to train the SOM is computed for all the frames.
#' @export
#'

#Function to compute and plot the silhouette profiles
plot.silhouette.profile <- function(SOM, Nclus, dist_clust="euclidean", clust_method="complete"){
    SOM.hc <- cutree(hclust(dist(SOM$codes[[1]], method=dist_clust), method=clust_method), Nclus)
    sil = cluster::silhouette(SOM.hc, dist(SOM$codes[[1]]))
    plot(sil, main="", do.clus.stat = FALSE, do.n.k = FALSE)
    abline(v=mean(sil[,3]), col='red', lty=3, lwd=2)
}
