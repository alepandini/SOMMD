# Cluster pathways
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM a kohonen SOM object. 
#' @param start the vector specifying the starting frame of each replicas
#' @param end the vector specifying the ending frame of each replicas
#' @param method the method to be passed to hclust for the clustering
#'
#' @return representatives a vector of frames representatives of each neuron 
#' @export
#'
#' @examples


cluster.pathways <- function(SOM, start, end, time.dep="independent", method="complete"){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check that the number of replicas is > 1
    if(length(start)<2){
        stop("start vector should specify the start of at least 2 replicas")
    }
    #check consistency of start and end vectors
    if(length(start) != length(end)){
        stop("start vector and end vector must have the same length")
    }
    #check consistency of start and end vectors
    if( length(which(start>end)) > 0 ){
        stop(paste("according to start and end vectors, replica", which(start>end)[1], 
                    "start at frame", start[which(start>end)], 
                    "and end at frame", end[which(start>end)], 
                    "which is not possible", sep=' '))
    }
    if(time.dep != "dependent" & time.dep != "independent"){
        stop("time.dep must be one between dependent or independent")
    }
    paths <- matrix(NA, nrow=max(end-start)+1, ncol=length(start))
    for(i in 1:length(start)){
        paths[1:((end[i]-start[i])+1), i] <- SOM$unit.classif[start[i]:end[i]]
    }   
    mat <- matrix(0, ncol=length(start), nrow=length(start))
    if(time.dep=="dependent"){
        if(sum(((end-start)-max(end-start))==0) != length(start)){
            warning("You are trying to use time dependent clustering on replicas of multiple length")
        }
        for(i in 1:length(start)){
            for(j in 1:length(start)){
                mat[i,j] <- dist_path_TimeDep(na.omit(paths[,i]), na.omit(paths[,j]), SOM$grid$pts)
            }
        }
    }
    if(time.dep=="independent"){
        for(i in 1:length(start)){
            for(j in 1:length(start)){
                mat[i,j] <- dist_path_TimeIndep(na.omit(paths[,i]), na.omit(paths[,j]), SOM$grid$pts)
            }
        }
    }
    path.clust <- hclust(as.dist(mat), method=method)
    return(path.clust)
}


#### Additional Functions used by previous function
#Function to compute the distance between two paths in a time-dependent mode
dist_path_TimeDep <- function(A, B, GRID){
    #A and B are two vectors of the same length containing the path through neurons while grid is the SOM$grid$pts
    D <- NULL
    for(i in 1:length(A)){
        D <- c(D, dist(rbind(GRID[A[i],], GRID[B[i],]), method="euclidean"))
    }
    return(sum(D)/length(A))
}

#Function to compute the distance between two paths in a time-independent mode
dist_path_TimeIndep <- function(A, B, GRID){
    #A and B are two vectors of the same length containing the path through neurons while grid is the SOM$grid$pts
    D1 <- NULL
    for(i in 1:length(A)){
        D1 <- c(D1, min(as.matrix(dist(rbind(GRID[A[i],], GRID[unique(B),]), method="euclidean", upper=TRUE, diag=TRUE))[1,-1]))
    }
    D2 <- NULL
    for(i in 1:length(B)){
        D2 <- c(D2, min(as.matrix(dist(rbind(GRID[B[i],], GRID[unique(A),]), method="euclidean", upper=TRUE, diag=TRUE))[1,-1]))
    }
    D1 <- sum(D1)/length(A)
    D2 <- sum(D2)/length(B)
    return(max(D1,D2))
}
