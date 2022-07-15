# Write a pdb file for the representative frame of a neuron
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param SOM a kohonen SOM object. 
#' @param filename the name of the pdb file that will be written out. 
#' @param traj the trajectory file that trained the SOM. 
#' @param neuron the neuron for which the representative conformation will be extracted and written to a PDB file
#' @param stride the stride that was applied to the traj.
#' @param append whether to append the group to an existing ndx file, or to overwrite it (if exist)
#'
#' ### WARNING: MISSING A CHECK FOR THE TRAJECTORY
#' @return 
#' @export
#'
#' @examples
#' #TODO Not added yet

extract_neur_representative <- function(SOM, filename, pdb, traj, neuron, stride=1, append=FALSE){
    #check whether SOM is a kohonen object
    if(inherits(SOM, "kohonen")==FALSE){
        stop("SOM must be a kohonen object")
    }
    #Check that neuron is numeric
    if(inherits(neuron, "numeric")==FALSE){
        stop("neuron must be a number")
    }
    #Check that the number of frames of the SOM and of the trajectory are the same according to the stride
    if(inherits(neuron, "numeric")==FALSE){
        stop("neuron must be a number")
    }
    #Check that neuron is a single number
    if(length(neuron > 1)){
        stop("Plese select a single neuron")
    }
    #Check that neuron exist
    if(neuron %in% c(1:nrow(SOM$grid$pts))==FALSE){
        stop(paste("Selected neuron: ", neuron, " does not exist, please select a neuron in the range 1-", nrow(SOM$grid$pts), sep=''))
    }
    #chek if the pdb is a pdb file 
    if(is.pdb(pdb)==FALSE){
        stop("Plese provide as pdb a pdb file read with bio3d")
    }
    #chek if the pdb have the same dimension of the trj
    if(nrow(pdb$atom) != dim(traj$coords)[1]){
        stop(paste("traj (", dim(traj$coords)[1] , ") and pdb (", nrow(pdb$atom), ") files do not have the same number of atoms", sep=''))
    }
    SET <- which(SOM$unit.classif==neuron)
    if(length(SET)==0){
        stop("Selected neuron does not contain frames")
    } else{
        REPRESENTATIVE <- SET[which(SOM$distances[SET]==min(SOM$distances[SET]))[1]]
    }
    C <- traj$coords[,,seq(1, dim(traj$coords)[3], by=stride)][,,REPRESENTATIVE]*10
    pdb$atom$x <- C[,1]
    pdb$atom$y <- C[,2]
    pdb$atom$z <- C[,3]
    write.pdb(pdb, file=filename, xyz=matrix(rbind(C[,1], C[,2], C[,3]), nrow=1))
}



#Function to select the neuron representative of cluster CL
Select_representative <- function(CENTROID, SOM, SOM.hc, CL){
    FR <- which(SOM.hc==CL)
    DistCentroid <- apply(SOM$codes[[1]], 1, COMPUTE_DISTANCE, V2=CENTROID[,CL])
    ReprNeuron <- which(DistCentroid==min(DistCentroid[FR]))
    return(ReprNeuron)
}

#Function to compute the representative frame for each neuron
NeuronRepres <- function(SOM){
    REPRESENTATIVE <- NULL
    for(i in 1:nrow(SOM$grid$pts)){
        SET <- which(SOM$unit.classif==i)
        if(length(SET)==0){
            REPRESENTATIVE <- c(REPRESENTATIVE, 0)
        } else{
            REPRESENTATIVE <- c(REPRESENTATIVE, SET[which(SOM$distances[SET]==min(SOM$distances[SET]))[1]])
        }
    }
    return(REPRESENTATIVE)
}
