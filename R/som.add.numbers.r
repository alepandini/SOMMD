#' Function to plot the silhouette profile for the Nclus cluster of the SOM neurons
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM: the SOM object 
#' @param scale: a number to scale up or down the size of the text
#' @param col: the color of the text
#'
#' @export
#'

som.add.numbers <- function(SOM, scale=1, col="black"){
    X <- NULL
    Y <- NULL
    for(i in c(1:nrow(SOM$grid$pts))){
        X <- SOM$grid$pts[i,1]
        Y <- SOM$grid$pts[i,2]
        text(x=X, y=Y, labels=i, cex=((scale*13)/SOM$grid$xdim), xpd=TRUE, col=col)
    }
}
