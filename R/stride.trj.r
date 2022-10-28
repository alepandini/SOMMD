# Apply a stride to a trj object to reduce the number of frames
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param traj a trj object. 
#' @param frame the frame to extract.
#'
#' @return a pdb object of the selected frame
#' @export
#'
#' @examples
#' #TODO Not added yet


stride.trj <- function(traj, stride){
    #Check that the trajectory is of class trj:
    if(class(traj)!="trj"){
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
    stride_traj <- traj
    NFRAME <- dim(traj$coord)[3]
    SEQ <- seq(1,NFRAME,by=stride)
    #This is a variable to store a warning to print (avoid to print warning at every for cycle
    WARN1 <- FALSE
    WARN2 <- FALSE
    for(i in 1:length(traj$start)){
        RF <- which(SEQ >= traj$start[i] & SEQ <= traj$end[i])
        if(length(RF)==0){
            WARN1 <- TRUE
        }
        if(length(RF)==1){
            WARN2 <- TRUE
        }
        stride_traj$start[i] <- head(RF, 1)
        stride_traj$end[i] <- tail(RF, 1) 
    }
    if(WARN1){
        warning("Using this stride some of your simulation parts remain with no frame")
    } else{
        if(WARN2){
            warning("Using this stride some of your simulation parts remain with a single frame")
        }
    }
    stride_traj$coord <- traj$coord[,,SEQ]
    stride_traj$call <- sys.call()
    return(stride_traj)
}
