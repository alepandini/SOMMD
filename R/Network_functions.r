#' Assign new data to a trained SOM
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM a trained SOM
#' @param X a data set with the same number of features of the dataset used to train the SOM
#'
#' @return MAT
#' @export
#'

###The following are all functions to compute a transition matrix from a SOM object and obtain an igraph network

#Compute the transition matrix starting from a vector of subsequent classifications
Compute_Transition_Matrix <- function(classif, starts){
    #Remove transitions across the replicas
    classif[starts] <- 0
    #Compute the probability of passing from neuron i to neuron j
    TRANS <- matrix(0, ncol=max(classif), nrow=max(classif))
    for(i in 1:max(classif)){
        #Total number of frame assigned to the neuron i
        TOTAL <- length(which(classif==i))
        #Neurons to which the neuron i has evolved to
        PASSAGE <- classif[which(classif==i)+1]
        if(TOTAL > 0){
            for(j in 1:max(classif)){
                NNN <- length(which(PASSAGE==j))
                    TRANS[i,j] <- NNN
            }
        }
    }
    colnames(TRANS) <- paste("N_", seq(1:max(classif)), sep='')
    rownames(TRANS) <- paste("N_", seq(1:max(classif)), sep='')
    return(TRANS)
}

#Function to convert a transition matrix to a network
Matrix2Network <- function(TRANS){
    #Create transition network
    d <- NULL
    for(i in 1:dim(TRANS)[1]){
        for(j in 1:dim(TRANS)[2]){
            if(TRANS[i,j]>0){
                d <- rbind(d, c(i, j, TRANS[i,j]))
            }
        }
    }
    return(d)
}

#Function to remove the diagonal from a network
Network_noDiagonal <- function(d){
    #Remove elements on the diagonal
    D <- NULL
    for(i in 1:nrow(d)){
        if(d[i,1]!=d[i,2]){
            D <- rbind(D, d[i,])
        }
    }
    return(D)
}

#Function to convert a network dataframe to an igraph graph
Network2Graph <- function(D, SOM, SOM.hc, COL.SCALE){
    POPULATION <- NULL
    N_NEUR <- nrow(SOM$codes[[1]])
    for(i in 1:N_NEUR){
        POPULATION <- c(POPULATION, length(which(SOM$unit.classif==i)))
    }
    #Create dataframe for nodes
    nodes <- cbind(c(1:nrow(SOM$grid$pts)), SOM.hc, POPULATION)
    colnames(nodes) <- c("node", "CLUSTER", "POPULATION")
    #Create Igraph Network
    colnames(D) <- c("V1","V2","weight")
    net <- igraph::graph_from_data_frame(d=D, vertices=nodes, directed=T)
    #Set some properties of the graph
    igraph::V(net)$color <- COL.SCALE[igraph::V(net)$CLUSTER]
    igraph::V(net)$size <- log(igraph::V(net)$POPULATION)*3
    Isolated = which(igraph::degree(net)==0)
    igraph::V(net)$size[Isolated] <- 0
    igraph::E(net)$width <- (log(D[,3])/max(log(D[,3])))*4
    W <- NULL
    for(i in 1:nrow(D)){
        P <- POPULATION[D[i,1]]
        W <- c(W, D[i,3]/P)
    }
    W <- -log(W) 
    igraph::E(net)$weight <- W
    return(net)
}

plot.network.SOM <- function(net, SOM, vertex.size=1, edge.size=1, edge.arrow.size=1, edge.curved=0.2, label=TRUE){
    edge.start <- igraph::ends(net, es=igraph::E(net), names=F)[,1]
    edge.col <- igraph::V(net)$color[edge.start]
    Isolated = which(igraph::degree(net)==0)
    igraph::V(net)$size[Isolated] <- 0
    N_NEUR <- nrow(SOM$grid$pts)
    VLAB <- seq(1,N_NEUR)
    VLAB[Isolated] <- " "
    igraph::V(net)$size <- igraph::V(net)$size*vertex.size
    igraph::E(net)$width <- igraph::E(net)$width*edge.size
    if(label==TRUE){
        plot(net, edge.arrow.size=igraph::E(net)$width*(edge.arrow.size/10), edge.curved=edge.curved, edge.color=edge.col, layout=SOM$grid$pts, vertex.label=VLAB)
    } else{
        plot(net, edge.arrow.size=igraph::E(net)$width*(edge.arrow.size/10), edge.curved=edge.curved, edge.color=edge.col, layout=SOM$grid$pts, vertex.label="")
    }
}

map2color<-function(x, pal, limits=NULL, na.col="grey"){
    na.vals <- which(is.na(Neur.avg.d)==TRUE)
    x2 <- x[-na.vals]
    if(is.null(limits)) limits=range(x2)
    COL <- pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
    COL[na.vals] <- na.col
    return(COL)
}
