#' @title Compute transition matrix
#' @description Compute the transition matrix starting from a vector of subsequent classifications
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param SOM a kohonen object on which transitions between neurons will be computed
#' @param start a vector containing the start frames of each replica (usually contained in trj$start if replicas were merged with cat_trj)
#' @return A matrix of pairwise transitions between neurons
#' @export
#' @examples
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #Compute transition Matrix
#' tr_mat <- comp.trans.mat(som_model, start = 1)
comp.trans.mat <- function(SOM, start=1){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    classif <- SOM$unit.classif
    N_states <- nrow(SOM$codes[[1]])
#   Check all start values are within the length of classif
    if(sum((start-length(classif))>0) != 0){
        stop("some start values exceed the length of classif")
    }
    toremove <- start-1
    toremove <- which(toremove > 0)
    #Remove transitions across the replicas
    classif[toremove] <- 0
    #Compute the probability of passing from neuron i to neuron j
    trans <- matrix(0, ncol=N_states, nrow=N_states)
    for(i in 1:N_states){
        #Total number of frame assigned to the neuron i
        total <- length(which(classif==i))
        #Neurons to which the neuron i has evolved to
        passage <- classif[which(classif==i)+1]
        if(total > 0){
            for(j in 1:N_states){
                NNN <- length(which(passage==j))
                    trans[i,j] <- NNN
            }
        }
    }
    colnames(trans) <- paste("N_", seq(1:N_states), sep='')
    rownames(trans) <- paste("N_", seq(1:N_states), sep='')
    return(trans)
}


#' @title Convert transition matrix to an igraph object
#' @description Function to convert a transition matrix to an igraph object
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param trans a transition matrix (usually obtained from comp.trans.mat)
#' @param SOM a kohonen object that form the network
#' @param SOM.hc a vector of cluster assignment for SOM neurons
#' @param col.set a vector of colors used for the SOM clusters
#' @param diag boolean condition to include diagonal elements
#' @return An igraph object, with SOM properties annotated
#' @export
#' @examples
#' #Read example SOM data
#' som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
#' #Divide the SOM in the selected number of clusters
#' som_cl <- cutree(hclust(dist(som_model$codes[[1]], method="euclidean"), method="complete"), 4)
#' #Compute transition matrix
#' tr_mat <- comp.trans.mat(som_model, start = 1)
#' #Define a set of colors
#' colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ffff88", "#6a3d9a")
#' #Create graph object
#' net <- matrix2graph(tr_mat, som_model, som_cl, colors, diag=FALSE)
matrix2graph <- function(trans, SOM, SOM.hc, col.set, diag=FALSE){
#   Check that trans have the shape of a transition matrix
    if( nrow(trans) != ncol(trans) ){
        stop("number of row and columns of trans must be the same")
    }
#   Check that the values of the transition matrix are all equal or greater than zero
    if( length(which(trans < 0)) > 0 ){
        stop("trans cannot have negative numbers")
    }
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check whether SOM.hc is numeric
    if((is.numeric(SOM.hc) == FALSE)){
        stop("SOM.hc must be the vector of cluster assignment as obtained for example by hclust")
    }
    #check if length of SOM.hc is compatible with SOM
    if((length(SOM.hc) != nrow(SOM$grid$pts))){
        stop(paste("SOM.hc have", length(SOM.hc), "elements, but SOM have", nrow(SOM$grid$pts), "neurons", sep=' '))
    }
    #check whether diag is logi
    if(is.logical(diag) == FALSE){
        stop("diag must be TRUE or FALSE")
    }
    #Create transition network
    d <- NULL
    for(i in 1:dim(trans)[1]){
        for(j in 1:dim(trans)[2]){
            if(trans[i,j]>0){
                d <- rbind(d, c(i, j, trans[i,j]))
            }
        }
    }
    #Remove elements on the diagonal
    if(diag==FALSE){
      d.nodiag <- NULL
      for(i in 1:nrow(d)){
          if(d[i,1]!=d[i,2]){
              d.nodiag <- rbind(d.nodiag, d[i,])
          }
      }
      d <- d.nodiag
    }
    pop <- NULL
    N.neur <- nrow(SOM$codes[[1]])
    for(i in 1:N.neur){
        pop <- c(pop, length(which(SOM$unit.classif==i)))
    }
    #Create dataframe for nodes
    nodes <- cbind(c(1:nrow(SOM$grid$pts)), SOM.hc, pop)
    colnames(nodes) <- c("node", "cluster", "population")
    #Create Igraph Network
    colnames(d) <- c("V1","V2","weight")
    net <- igraph::graph_from_data_frame(d=d, vertices=nodes, directed=T)
    #Set some properties of the graph
    igraph::V(net)$color <- col.set[igraph::V(net)$cluster]
    igraph::V(net)$size <- log(igraph::V(net)$population)*3
    isolated = which(igraph::degree(net)==0)
    igraph::V(net)$size[isolated] <- 0
    igraph::E(net)$width <- (log(d[,3])/max(log(d[,3])))*4
    W <- NULL
    for(i in 1:nrow(d)){
        P <- pop[d[i,1]]
        W <- c(W, d[i,3]/P)
    }
    W <- -log(W)
    igraph::E(net)$weight <- W
    return(net)
}


#' @title Map the property vector to colours
#' @description Function map a numeric vector of a property to a vector of colors for that property according to that property value.
#' @param x a numeric vector
#' @param pal a color palette
#' @param limits the values of the extremes for the colorscale
#' @param na.col the color that will be assigned to the na.values of the vector
#' @return A vector with colors proportional to the values of x
#' @export
#'
map.color <- function(x, pal, limits=NULL, na.col="grey"){
    if( is.numeric(x) == FALSE){
        stop("x must be a numeric vector")
    }
    na.vals <- which(is.na(x)==TRUE)
    if(length(na.vals)>0){
        x2 <- x[-na.vals]
    } else{
        x2 <- x
    }
    if(is.null(limits)) limits=range(x2)
    COL <- pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
    COL[na.vals] <- na.col
    return(COL)
}
