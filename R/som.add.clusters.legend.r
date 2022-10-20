#' Function to plot the silhouette profile for the Nclus cluster of the SOM neurons
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM: the SOM object 
#' @param scale: a number to scale up or down the size of the text
#' @param col: the color of the text
#'
#' @export
#'

#Function to create a vector of two letters identifiers
TwoLetters <- function(LETTERS=LETTERS){
    LET <- NULL
    for(i in LETTERS){
        for(j in LETTERS){
        LET <- c(LET, paste(i, j, sep=''))
        }
    }
    return(LET)
}

#Function to apply a legend of clusters to a SOM map image
som.add.clusters.legend <- function(NCLUS, COL.SCALE){
    if(NCLUS < length(LETTERS)){
        LEG_LAB <- paste("Cluster ", LETTERS, sep=" ")[1:NCLUS]
        CX <- 0.8
    } else{
        LET <- TwoLetters(LETTERS)
        LEG_LAB <- paste("Cluster ", LET, sep=" ")[1:NCLUS]
        CX <- 0.8
    }
    MyBorders = rep("black", NCLUS)
    legend("right", legend=LEG_LAB[1:NCLUS], fill=COL.SCALE[1:NCLUS], ncol=1, xpd=TRUE, cex=CX, bty="n", border=MyBorders) #or pch=22
}

