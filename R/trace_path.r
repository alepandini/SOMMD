#' Function trace pathway sampled on the SOM
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM: the SOM object 
#' @param start: a vector containing the start frames of each replica (usually contained in trj$start if replicas were merged with cat_trj)
#' @param end: a vector containing the end frames of each replica (usually contained in trj$end if replicas were merged with cat_trj)
#' @param N: The portion of simulation that one one to plot (only frames between trj$start[N] and trj$end[N] will be plotted)
#' @param draw_stride: used to plot the pathways with a stride (usefull for very complex pathways)
#' @param scale: a number to scale up or down the size of the text
#'
#' @export
#'

#Function to draw pathways over the SOM
trace_path <- function(SOM, start=1, end=length(SOM$unit.classif), N=1, draw_stride=1, scale=1){
    X <- NULL
    Y <- NULL
    BWR <- colorRampPalette(c("blue", "white", "red"))
#     trj_frames <- seq(head(start, 1), tail(end, 1))
    trj_frames_stride <- seq(head(start, 1), tail(end, 1))
    rep_frames <- which(trj_frames_stride >= start[N] & trj_frames_stride <= end[N])
    for(i in rep_frames[seq(1, length(rep_frames), by=draw_stride)]){
        u <- SOM$unit.classif[i]
        X <- c(X, SOM$grid$pts[u,1])
        Y <- c(Y, SOM$grid$pts[u,2])
    }
    points(X,Y, pch=16, cex=(25*scale)/SOM$grid$xdim, xpd=T)
    points(X,Y, pch=16, col=BWR(length(X)), cex=(18*scale)/SOM$grid$xdim, xpd=T)
    lines(X,Y, pch=16, lwd=5*scale, xpd=T)
}
