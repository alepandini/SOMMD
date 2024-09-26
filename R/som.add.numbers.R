#' @title Add Neuron Numbering
#' @description Add the neuron numbering scheme to the SOM plot
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM the SOM object
#' @param scale a number to scale up or down the size of the text
#' @param col the color of the text
#' @return Called for its effect.
#' @export
#' @examples
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #Plot the som
#' plot(som_model, type = "count", bgcol=c("red", "blue", "yellow", "green"), shape='straight')
#' #Add neuron numbers on the som
#' som.add.numbers(som_model, scale=0.5, col="black")
#'
som.add.numbers <- function(SOM, scale=1, col="black"){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    X <- NULL
    Y <- NULL
    #Add the neuron number to the SOM plot
    for(i in c(1:nrow(SOM$grid$pts))){
        X <- SOM$grid$pts[i,1]
        Y <- SOM$grid$pts[i,2]
        graphics::text(x=X, y=Y, labels=i, cex=((scale*13)/SOM$grid$xdim), xpd=TRUE, col=col)
    }
}
