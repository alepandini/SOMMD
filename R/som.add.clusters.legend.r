#' @title Add legend clusters
#' @description Function to apply a legend of clusters to a SOM map image
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param N.clus the number of clusters to which put the legent
#' @param color.scale the color scale used for the image
#' @export
#' @examples
#' som.add.clusters.legend(N.clus=8, color.scale=COL.SCALE)
#'
som.add.clusters.legend <- function(N.clus, color.scale){
    #If the number of cluster is greater than the number of letters use a Two Letters code
    if(N.clus < length(LETTERS)){
        LEG_LAB <- paste("Cluster ", LETTERS, sep=" ")[1:N.clus]
        CX <- 0.8
    } else{
        LET <- TwoLetters(LETTERS)
        LEG_LAB <- paste("Cluster ", LET, sep=" ")[1:N.clus]
        CX <- 0.8
    }
    #Add the legend to the plot
    MyBorders = rep("black", N.clus)
    graphics::legend("right", legend=LEG_LAB[1:N.clus], fill=color.scale[1:N.clus], ncol=1, xpd=TRUE, cex=CX, bty="n", border=MyBorders) #or pch=22
}
