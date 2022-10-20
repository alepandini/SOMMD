# Extract a trj frame to a pdb object
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


trj2pdb <- function(traj, frame, filename){
    #Check that the trajectory is of class trj:
    if(class(traj)!="trj"){
        stop("The trajectory should be an object with class trj")
    }
    #Check that the frame is a number
    if(is.numeric(frame)==FALSE){
        stop("frame must be a number")
    }
    #Check that frame is a single number
    if(length(frame) > 1){
        stop("Plese select a single frame to extract")
    }
    #Check that the frame exist
    if(frame %in% c(1:dim(traj$coord)[3])==FALSE){
        stop(paste("Selected frame: ", frame, " does not exist, please select a neuron in the range 1-", dim(traj$coord)[3], sep=''))
    }
    sink(filename)
    cat(paste("Frame ", frame, "\n", sep=''))
    cat("Written with SOMMD trj2pdb function\n")
    for(i in 1:nrow(traj$top)){
        if(is.na(traj$top$chain[i])){
            traj$top$chain[i] <- " "
        }
        cat(sprintf("%-6s%5d%1s%-4s%4s%2s%4.0f%12.3f%8.3f%8.3f%6.2f%6.2f\n", 
                       "ATOM", i, " ", traj$top$elety[i], traj$top$resid[i], traj$top$chain[i], 
                       traj$top$resno[i], traj$coord[i,1,frame]*10, traj$coord[i,2,frame]*10,  
                       traj$coord[i,3,frame]*10, 1, 0))
    }
    cat("END")
    sink()
}
