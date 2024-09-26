#' @title Extract frame to pdb
#' @description Extract a trj frame to a pdb object
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param trj a trj object.
#' @param frame the frame to extract.
#' @param filename for the output pdb file
#' @return a pdb object of the selected frame
#' @return Called for its effect.
#' @export
#' @examples
#' \donttest{
#' # Read the trajectory
#' trj <- read.trj(trjfile = system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD"),
#'   topfile = system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' # Write the pdb file for a specific frame
#' trj2pdb(trj = trj, frame=5, filename = tempfile(fileext = '.pdb' ))
#' }
#'
trj2pdb <- function(trj, frame, filename){
    #Check that the trajectory is of class trj:
    if(!methods::is(trj,"trj")){
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
    if(frame %in% c(1:dim(trj$coord)[3])==FALSE){
        stop(paste("Selected frame: ", frame, " does not exist, please select a neuron in the range 1-", dim(trj$coord)[3], sep=''))
    }
    sink(filename)
    cat(paste("Frame ", frame, "\n", sep=''))
    cat("Written with SOMMD trj2pdb function\n")
    for(i in 1:nrow(trj$top)){
        if(is.na(trj$top$chain[i])){
            trj$top$chain[i] <- " "
        }
        #Write in the pdb format
        cat(sprintf("%-6s%5d%1s%-4s%4s%2s%4.0f%12.3f%8.3f%8.3f%6.2f%6.2f\n",
                       "ATOM", i, " ", trj$top$elety[i], trj$top$resid[i], trj$top$chain[i],
                       trj$top$resno[i], trj$coord[i,1,frame]*10, trj$coord[i,2,frame]*10,
                       trj$coord[i,3,frame]*10, 1, 0))
    }
    cat("END")
    sink()
}
