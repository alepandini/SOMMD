#' Coordinate superposition
#' 
#' Coordinate superposition with the Kabsch algorithm.
#' This function make use of the bio3d fit.xyz function to align a SOMMD trj object.
#' If ref is not specified, the trj object is aligned to the first frame of the simulation, otherwise it is aligned to the reference input object.
#
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param trj an object with class trj
#' @param ref a pdb or gro object to be used as reference
#' @param trj.inds a vector of indices that selects the trj atoms upon which fitting should be based. If not specified all atoms will be used.
#' @param ref.inds a vector of indices that selects the ref atoms upon which fitting should be based. If not specified all atoms will be used.
#'
#' @return an aligned trj object  
#' @export
#'
#' @examples
#'
#' # Fit a trajectory to the first frame based on alpha carbons:
#' ca.inds <- which(trj$top$elety=="CA")
#' trj.fit <- fit(trj, trj.inds=ca.inds)

fit <- function(trj, ref = NULL, trj.inds = NULL, ref.inds = NULL){
    if(class(trj)!="trj"){
        stop("The trajectory should be an object with class trj")
    }
    if(is.null(ref) & is.null(ref.inds) == FALSE){
        warning("A ref.inds was provided without any ref. Using first frame as reference with trj.inds")
    }
    if(is.null(ref)){
        ref <- trj$coord[,,1]
        ref.inds <- trj.inds
    } else{
        if(class(ref) != "pdb" & class(ref) != "gro"){
            stop("ref must be an object with class pdb or gro")
        }
        ref <- cbind(ref$atom$x, ref$atom$y, ref$atom$z)
    }
    if(is.null(trj.inds)){
        trj.inds <- seq(1, dim(trj$coord)[1])
    } else{
        if(is.numeric(trj.inds) == FALSE){
            stop("trj.inds must be a numeric vector of atom indeces")
        }
        if(max(trj.inds) > dim(trj$coord)[1]){
            stop("trj.inds contain atom indeces that exceed the number of atoms of trj")
        }
    }
    if(is.null(ref.inds)){
        ref.inds <- seq(1, nrow(ref))
    } else{
        if(is.numeric(ref.inds) == FALSE){
            stop("ref.inds must be a numeric vector of atom indeces")
        }
        if(max(ref.inds) > nrow(ref)){
            stop("ref.inds contain atom indeces that exceed the number of atoms of ref")
        }
    }
    fixed <- c(t(ref[ref.inds]))
    
    mobile <- trj2xyz(trj)
    fit_output <- bio3d::fit.xyz(fixed=ref, mobile=mobile,
                                 fixed.inds = ref.inds,
                                 mobile.inds = trj.inds)
    nframes <- dim(fit_output)[1]
    ncoords <- dim(fit_output)[2]
    natoms <- ncoords/3
    fit_output[,] <- t(fit_output[,])
    dim(fit_output) <- c(nframes * ncoords, 1)
    dim(fit_output) <- c(3, natoms, nframes)
    trj$coord <- aperm(fit_output, c(2,1,3))
    return(trj)
}


#' Convert Trajectory to xyz
#' 
#' Convert the trj coordinates 3D-array in a 2D matrix.
#
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @param trj an object with class trj
#'
#' @return a xyz matrix with frames on rows and coordinates as columns
#' @export
#' @examples
#'
#' trj2xyz(trj)

trj2xyz <- function(trj, inds=NULL){
    if(class(trj)!="trj"){
        stop("The trajectory should be an object with class trj")
    }
    if(is.null(inds)){
        inds <- seq(1, dim(trj$coord)[1])
    } else{
        if(is.numeric(inds) == FALSE){
            stop("inds must be a numeric vector of atom indeces")
        }
        if(max(inds) > dim(trj$coord)[1]){
            stop("inds contain atom indeces that exceed the number of atoms of trj")
        }
    }
    xt <- aperm(trj$coord[inds,,], c(3,2,1))
    xt2 <- apply(xt, 1, c)
    xyz <- aperm(xt2, c(2,1))
    return(xyz)
}

