#' Function to read a set of xtc file, and concatenate them.

#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param FILES the xtc files to be read
#'
#' @return TRJ a list, with $coords being the coordinates of the simulation and $starts being the starting frame of each replica (usefull to have it for some analysis)
#' @export
#'
#' @examples
#' #TODO Not added yet

read_replicas <- function(FILES){
    coords <- NULL
    #A counter for the number of frames
    S <- 1
    starts <- NULL
    for(FILE in FILES){
        #Also record for starting frame of each replica
        starts <- c(starts, S)
        coords <- abind::abind(coords, rio_read_xtc(FILE), along = 3)
        S <- S+rio_read_xtc_nframes(FILE)
    }
    TRJ <- list(coords=coords, starts=starts)
    return(TRJ)
}
