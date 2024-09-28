#' @title Trace pathway
#' @description Function trace pathway sampled on the SOM
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM the SOM object
#' @param start a vector containing the start frames of each replica (usually contained in trj$start if replicas were merged with cat_trj)
#' @param end a vector containing the end frames of each replica (usually contained in trj$end if replicas were merged with cat_trj)
#' @param N The portion of simulation that one want to plot
#' @param draw.stride used to plot the pathways with a stride (useful for very complex pathways)
#' @param pts.scale a number to scale up or down the size of the circles
#' @param lwd.scale a number to scale up or down the size of the lines
#' @return Called for its effect.
#' @export
#' @examples
#' # Read the trajectory
#' trj <- read.trj(trjfile = system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD"),
#'   topfile = system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #trace pathway sampled on the SOM
#' trace.path(som_model, start=trj$start, end=trj$end, N=1, pts.scale=0.5)
#'
trace.path <- function(SOM, start=1, end=length(SOM$unit.classif), N=1, draw.stride=1, pts.scale=1, lwd.scale=1){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    X <- NULL
    Y <- NULL
    BWR <- grDevices::colorRampPalette(c("blue", "white", "red"))
    trj.frames.stride <- seq(utils::head(start, 1), utils::tail(end, 1))
    #For the selected replica (N)
    rep.frames <- which(trj.frames.stride >= start[N] & trj.frames.stride <= end[N])
    #For every frame
    for(i in rep.frames[seq(1, length(rep.frames), by=draw.stride)]){
        #Compute the neuron position and store their coordinates in X and Y
        u <- SOM$unit.classif[i]
        X <- c(X, SOM$grid$pts[u,1])
        Y <- c(Y, SOM$grid$pts[u,2])
    }
    #draw the X and Y coordinates and the paths
    graphics::points(X,Y, pch=16, cex=(25*pts.scale)/SOM$grid$xdim, xpd=T)
    graphics::points(X,Y, pch=16, col=BWR(length(X)), cex=(18*pts.scale)/SOM$grid$xdim, xpd=T)
    graphics::lines(X,Y, pch=16, lwd=5*lwd.scale, xpd=T)
}
