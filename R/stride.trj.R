#' @title Stride a trj
#' @description Apply a stride to the frame of a trj object to reduce the number of frames
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param trj a trj object.
#' @param stride the stride to apply to the trajectory
#' @return An object of class trj with a frame every \code{stride}
#' @export
#' @examples
#' # Read the simulation
#' trj <- read.trj(trjfile = system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD"),
#'   topfile = system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' # keep a frame every 2 frame
#' trj_strd <- stride.trj(trj, 2)
#'
stride.trj <- function(trj, stride){
    #Check that the trajectory is of class trj:
    if(!methods::is(trj,"trj")){
        stop("The trajectory should be an object with class trj")
    }
    #Check that stride is a number
    if(is.numeric(stride)==FALSE){
        stop("stride should be a number")
    }
    #Check that stride is a single number
    if(length(stride) > 1){
        stop("stride should be a number")
    }
    stride_trj <- trj
    NFRAME <- dim(trj$coord)[3]
    SEQ <- seq(1,NFRAME,by=stride)
    #This is a variable to store a warning to print (avoid to print warning at every for cycle
    WARN1 <- FALSE
    WARN2 <- FALSE
    for(i in 1:length(trj$start)){
        #Check if parts of the simulations have at least 1 frame each
        RF <- which(SEQ >= trj$start[i] & SEQ <= trj$end[i])
        if(length(RF)==0){
            WARN1 <- TRUE
        }
        #Check if parts of the simulations have more than 1 frame each
        if(length(RF)==1){
            WARN2 <- TRUE
        }
        stride_trj$start[i] <- utils::head(RF, 1)
        stride_trj$end[i] <- utils::tail(RF, 1)
    }
    if(WARN1){
        warning("Using this stride some of your simulation parts remain with no frame")
    } else{
        if(WARN2){
            warning("Using this stride some of your simulation parts remain with a single frame")
        }
    }
    stride_trj$coord <- trj$coord[,,SEQ]
    stride_trj$call <- sys.call()
    return(stride_trj)
}
