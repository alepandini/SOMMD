#' Plot silhouette score
#'
#' Function to plot the silhouette score for the clustering of SOM neurons
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param SOM the SOM object to cluster
#' @param dist_clust the metric for the distance calculation
#' @param clust_method the method for the clustering (passed to the hclust function
#' @param interval the cluster number on which the silhouette score will be computed
#'
#' @return D the set of distances used to train the SOM is computed for all the frames.
#' @export
#'
#' @examples
#' plot.silhouette.score(SOM, clust_method="complete", intervall=seq(2,30))

#Function to compute and plot the silhouette profiles
plot.silhouette.score <- function(SOM, dist_clust="euclidean", clust_method="complete", interval=seq(2,30)){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check whether Nclus is lower than the number of neurons
    if(max(interval) > nrow(SOM$grid$pts) ){
        stop("The upper limit of interval cannot exceed the number of SOM neurons")
    }
    SIL <- NULL
    for(i in interval){
            SOM.hc <- cutree(hclust(dist(SOM$codes[[1]], method=dist_clust), method=clust_method), i)
            sil = cluster::silhouette(SOM.hc, dist(SOM$codes[[1]]))
        SIL <- c(SIL, mean(sil[,3]))
    }
    plot(interval, SIL, type='b', pch=19, lwd=1, xlab="Number of clusters", ylab='Average silhouettes')
    for(i in seq(0, max(interval), by=2)){
        abline(v=i, col="grey", lwd=0.5)
    }
}
