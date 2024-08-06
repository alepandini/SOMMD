#' @title Clustering of Pathways
#' @description Cluster pathways according to a time dependent or independent scheme
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#' @param SOM a kohonen SOM object.
#' @param start the vector specifying the starting frame of each replicas
#' @param end the vector specifying the ending frame of each replicas
#' @param time.dep choose whether to use time "dependent" or "independent" clustering of pathways
#' @param method the method to be passed to hclust for the clustering
#' @return representatives a vector of frames representatives of each neuron
#' @export
#' @examples
#' #Read trajectory
#' trj <- read.trj(trjfile = system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD"),
#'   topfile = system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' #Assign length of the replicas
#' trj$start <- seq(1, 25, by=5)
#' trj$end <- seq(5, 25, by=5)
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #Cluster Pathways using the time dependent algorithm
#' clus.paths.tdep <- cluster.pathways(som_model, start=trj$start, end=trj$end,
#'   time.dep="dependent")
#' #Cluster Pathways using the time independent algorithm
#' clus.paths.tindep <- cluster.pathways(som_model,
#'   start=trj$start, end=trj$end, time.dep="independent")
#'
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
    # Store paths in a matrix with every replica in columns
    paths <- matrix(NA, nrow=max(end-start)+1, ncol=length(start))
    for(i in 1:length(start)){
        paths[1:((end[i]-start[i])+1), i] <- SOM$unit.classif[start[i]:end[i]]
    }
    #mat is a matrix of distances
    mat <- matrix(0, ncol=length(start), nrow=length(start))
    #Check if replicas are of same length in case of time dependent clustering
    if(time.dep=="dependent"){
        if(sum(((end-start)-max(end-start))==0) != length(start)){
            warning("You are trying to use time dependent clustering on replicas of multiple length")
        }
    }
    #Compute paths distances
    for(i in 1:length(start)){
        for(j in 1:length(start)){
            mat[i,j] <- dist.paths(stats::na.omit(paths[,i]), stats::na.omit(paths[,j]), SOM$grid$pts, time.dep=time.dep)
        }
    }
    #Cluster the distance matrix
    path.clust <- stats::hclust(stats::as.dist(mat), method=method)
    return(path.clust)
}

#' @title Distance between two paths
#' @description Function to compute the distance between two paths
#' @param A a vector of frame neuron assignment for replica 1
#' @param B a vector of frame neuron assignment for replica 2
#' @param SOM.grid the grid coordinate of the SOM neurons
#' @return the distance between the two pathways
#' @noRd
dist.paths <- function(A, B, SOM.grid, time.dep='independent'){
    if(time.dep != "dependent" & time.dep != "independent"){
        stop("time.dep must be one between dependent or independent")
    }
    if(time.dep=="independent"){
        #A and B are two vectors of the same length containing the path through neurons while SOM.grid is the SOM$grid$pts
        #Compute at every step of A the distance from the closest B neuron
        D1 <- NULL
        for(i in 1:length(A)){
            D1 <- c(D1, min(as.matrix(stats::dist(rbind(SOM.grid[A[i],], SOM.grid[unique(B),]), method="euclidean", upper=TRUE, diag=TRUE))[1,-1]))
        }
        #Compute at every step of B the distance from the closest A neuron
        D2 <- NULL
        for(i in 1:length(B)){
            D2 <- c(D2, min(as.matrix(stats::dist(rbind(SOM.grid[B[i],], SOM.grid[unique(A),]), method="euclidean", upper=TRUE, diag=TRUE))[1,-1]))
        }
        #The distance between the two paths would be the average of the two distances
        D1 <- sum(D1)/length(A)
        D2 <- sum(D2)/length(B)
        return(max(D1,D2))
    } else{
        #Time dependent distance
        #A and B are two vectors of the same length containing the path through neurons while SOM.grid is the SOM$grid$pts
        D <- NULL
        #Compute the distance between the paths at evert step
        for(i in 1:length(A)){
            D <- c(D, stats::dist(rbind(SOM.grid[A[i],], SOM.grid[B[i],]), method="euclidean"))
        }
        return(sum(D)/length(A))
    }
}
