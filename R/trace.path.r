#' Trace pathway
#'
#' Function trace pathway sampled on the SOM
#'
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param SOM: the SOM object 
#' @param start: a vector containing the start frames of each replica (usually contained in trj$start if replicas were merged with cat_trj)
#' @param end: a vector containing the end frames of each replica (usually contained in trj$end if replicas were merged with cat_trj)
#' @param N: The portion of simulation that one want to plot (only frames between start[N] and end[N] will be plotted)
#' @param draw.stride: used to plot the pathways with a stride (usefull for very complex pathways)
#' @param pts.scale: a number to scale up or down the size of the circles
#' @param lwd.scale: a number to scale up or down the size of the lines
#'
#' @export
#'
#' @examples
#' trace_path(SOM, start=trj$start, end=trj$end, N=1, scale=0.5)

#Function to draw pathways over the SOM
trace.path <- function(SOM, start=1, end=length(SOM$unit.classif), N=1, draw.stride=1, pts.scale=1, lwd.scale=1){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    X <- NULL
    Y <- NULL
    BWR <- colorRampPalette(c("blue", "white", "red"))
#     trj_frames <- seq(head(start, 1), tail(end, 1))
    trj.frames.stride <- seq(head(start, 1), tail(end, 1))
    rep.frames <- which(trj.frames.stride >= start[N] & trj.frames.stride <= end[N])
    for(i in rep.frames[seq(1, length(rep.frames), by=draw.stride)]){
        u <- SOM$unit.classif[i]
        X <- c(X, SOM$grid$pts[u,1])
        Y <- c(Y, SOM$grid$pts[u,2])
    }
    points(X,Y, pch=16, cex=(25*pts.scale)/SOM$grid$xdim, xpd=T)
    points(X,Y, pch=16, col=BWR(length(X)), cex=(18*pts.scale)/SOM$grid$xdim, xpd=T)
    lines(X,Y, pch=16, lwd=5*lwd.scale, xpd=T)
}
