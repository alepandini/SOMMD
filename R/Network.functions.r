#' @title Compute transition matrix
#' @description Compute the transition matrix starting from a vector of subsequent classifications
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param classif a vector of neuron assigment (usually passed by SOM$unit.classif
#' @param start a vector containing the start frames of each replica (usually contained in trj$start if replicas were merged with cat_trj)
#' @return trans
#' @export
#'
comp.trans.mat <- function(classif, start=1){
#   Check that the classif is numeric
    if(is.numeric(classif)==FALSE){
        stop("classif must be a numeric vector")
    }
#   Check that the start is numeric
    if(is.numeric(start)==FALSE){
        stop("start must be a numeric vector")
    }
#   Check all start values are within the length of classif
    if(sum((start-length(classif))>0) != 0){
        stop("some start values exceed the length of classif")
    }
    #Remove transitions across the replicas
    classif[start] <- 0
    #Compute the probability of passing from neuron i to neuron j
    trans <- matrix(0, ncol=max(classif), nrow=max(classif))
    for(i in 1:max(classif)){
        #Total number of frame assigned to the neuron i
        total <- length(which(classif==i))
        #Neurons to which the neuron i has evolved to
        passage <- classif[which(classif==i)+1]
        if(total > 0){
            for(j in 1:max(classif)){
                NNN <- length(which(passage==j))
                    trans[i,j] <- NNN
            }
        }
    }
    colnames(trans) <- paste("N_", seq(1:max(classif)), sep='')
    rownames(trans) <- paste("N_", seq(1:max(classif)), sep='')
    return(trans)
}

#' @title Convert transition matrix to Network
#' @description Function to convert a transition matrix to a network
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param trans a transition matrix (usually obtained from comp.trans.mat)
#' @return The network in the form of a dataframe
#' @export
#'
Matrix2Network <- function(trans){
#   Check that trans have the shape of a transition matrix
    if( nrow(trans) != ncol(trans) ){
        stop("number of row and columns of trans must be the same")
    }
#   Check that the values of the transition matrix are all equal or greater than zero
    if( length(which(trans < 0)) > 0 ){
        stop("trans cannot have negative numbers")
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
    return(d)
}

#' @title Remove diagonal elements
#' @description Function to remove the diagonal from a network
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param d the network in the form of a dataframe
#' @return The network with no diagonal
#' @export
#'
rm.net.diag <- function(d){
#   Check that d have the shape of a network dataframe
    if( ncol(d) != 3 ){
        stop("d must be a network in the form of a dataframe (three columns)")
    }
    if( length(which(d[,3] < 0)) > 0 ){
        stop("d cannot have negative numbers in the third column")
    }
    #Remove elements on the diagonal
    d.nodiag <- NULL
    for(i in 1:nrow(d)){
        if(d[i,1]!=d[i,2]){
            d.nodiag <- rbind(d.nodiag, d[i,])
        }
    }
    return(d.nodiag)
}

#' @title convert a network to igraph object
#' @description Function to convert a network dataframe to an igraph object
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param d a network in the form of a dataframe
#' @param SOM a kohonen object that form the network
#' @param SOM.hc a vector of cluster assignment for SOM neurons
#' @param col.set a vector of colors used for the SOM clusters
#' @return The network as igraph object, with the SOM properties
#' @export
#'
Network2Graph <- function(d, SOM, SOM.hc, col.set){
#   Check that d have the shape of a network dataframe
    if( ncol(d) != 3 ){
        stop("d must be a network in the form of a dataframe (three columns)")
    }
    if( length(which(d[,3] < 0)) > 0 ){
        stop("d cannot have negative numbers in the third column")
    }
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #check whether SOM.hc is numeric
    if((is.numeric(SOM.hc) == FALSE)){
        stop("SOM must be a kohonen object")
    }
    #check if length of SOM.hc is compatible with SOM
    if((length(SOM.hc) != nrow(SOM$grid$pts))){
        stop(paste("SOM.hc have", length(SOM.hc), "elements, but SOM have", nrow(SOM$grid$pts), "neurons", sep=' '))
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
#' @return COL a vector with the same length of x, with colors proportional to the values of x
#' @export
#'
map2color <- function(x, pal, limits=NULL, na.col="grey"){
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
