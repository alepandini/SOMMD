#' Clustering of Pathways
#'
#' Cluster pathways according to a time dependent or independent scheme
#'
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM a kohonen SOM object. 
#' @param start the vector specifying the starting frame of each replicas
#' @param end the vector specifying the ending frame of each replicas
#' @param time.dep choose whether to use time "dependent" or "independent" clustering of pathways
#' @param method the method to be passed to hclust for the clustering
#'
#' @return representatives a vector of frames representatives of each neuron 
#' @export
#'
#' @examples 
#' # Cluster Pathways using the time dependent algorithm
#' cluster.pathways(SOM, start=trj$start, end=trj$end, time.dep="dependent")
#'
#' #Cluster Pathways using the time independent algorithm
#' cluster.pathways(SOM, start=trj$start, end=trj$end, time.dep="dependent")

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
    }
    for(i in 1:length(start)){
        for(j in 1:length(start)){
            mat[i,j] <- dist.paths(na.omit(paths[,i]), na.omit(paths[,j]), SOM$grid$pts, time.dep=time.dep)
        }
    }
    path.clust <- hclust(as.dist(mat), method=method)
    return(path.clust)
}



### Internal Function
#' Distance between two paths
#'
#' Function to compute the distance between two paths
#' @param A a vector of frame neuron assignment for replica 1
#' @param B a vector of frame neuron assignment for replica 2
#' @param SOM.grid the grid coordinate of the SOM neurons
#'
#' @return the distance between the two pathways
#' @noRd
dist.paths <- function(A, B, SOM.grid, time.dep='independent'){
    if(time.dep != "dependent" & time.dep != "independent"){
        stop("time.dep must be one between dependent or independent")
    }
    if(time.dep=="independent"){
        #A and B are two vectors of the same length containing the path through neurons while SOM.grid is the SOM$grid$pts
        D1 <- NULL
        for(i in 1:length(A)){
            D1 <- c(D1, min(as.matrix(dist(rbind(SOM.grid[A[i],], SOM.grid[unique(B),]), method="euclidean", upper=TRUE, diag=TRUE))[1,-1]))
        }
        D2 <- NULL
        for(i in 1:length(B)){
            D2 <- c(D2, min(as.matrix(dist(rbind(SOM.grid[B[i],], SOM.grid[unique(A),]), method="euclidean", upper=TRUE, diag=TRUE))[1,-1]))
        }
        D1 <- sum(D1)/length(A)
        D2 <- sum(D2)/length(B)
        return(max(D1,D2))
    } else{
        #A and B are two vectors of the same length containing the path through neurons while SOM.grid is the SOM$grid$pts
        D <- NULL
        for(i in 1:length(A)){
            D <- c(D, dist(rbind(SOM.grid[A[i],], SOM.grid[B[i],]), method="euclidean"))
        }
        return(sum(D)/length(A))
    }
}
