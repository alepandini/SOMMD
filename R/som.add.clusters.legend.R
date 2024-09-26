#' @title Add legend clusters
#' @description Function to apply a legend of clusters to a SOM map image
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param Nclus the number of clusters to which put the legent
#' @param color.scale the color scale used for the image
#' @return Called for its effect.
#' @export
#' @examples
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #Divide the SOM in the selected number of clusters
#' som_cl <- cutree(hclust(dist(som_model$codes[[1]], method="euclidean"), method="complete"), 4)
#' #Define a set of colors
#' colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ffff88", "#6a3d9a") 
#' #Plot the som with neurons colored according to clusters
#' plot(som_model, type = "mapping", bgcol=colors[som_cl], col=rgb(0,0,0,0), shape='straight', main="")
#' kohonen::add.cluster.boundaries(som_model, som_cl, lwd=5)
#' #Add legend to the plot
#' som.add.clusters.legend(Nclus=4, color.scale=colors)
#' 
som.add.clusters.legend <- function(Nclus, color.scale){
    #If the number of cluster is greater than the number of letters use a Two Letters code
    if(Nclus < length(LETTERS)){
        LEG_LAB <- paste("Cluster ", LETTERS, sep=" ")[1:Nclus]
        CX <- 0.8
    } else{
        LET <- TwoLetters(LETTERS)
        LEG_LAB <- paste("Cluster ", LET, sep=" ")[1:Nclus]
        CX <- 0.8
    }
    #Add the legend to the plot
    MyBorders = rep("black", Nclus)
    graphics::legend("right", legend=LEG_LAB[1:Nclus], fill=color.scale[1:Nclus], ncol=1, xpd=TRUE, cex=CX, bty="n", border=MyBorders) #or pch=22
}
