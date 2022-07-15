#Function to write a series of frames (frames) to a file in the ndx format
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param filename the name of the ndx file that will be written out. 
#' @param group the name of the group in the ndx file
#' @param frames the frame numbers that belong to the group
#' @param append whether to append the group to an existing ndx file, or to overwrite it (if exist)
#'
#' @return 
#' @export
#'
#' @examples
#' TODO Not added yet


frames_to_ndx <- function(filename, group, frames, append=FALSE){
    write(paste("[ ", group, " ]", sep=''), file=filename, append=append)
    NR <- as.integer(length(frames)/15)
    REST <- length(frames)-NR*15
    if(NR>0){
        M <- t(matrix(frames[1:(NR*15)], ncol=NR))
        for(i in 1:nrow(M)){
            L <- paste(M[i,], collapse=' ')
            write(L, file=filename, append=TRUE)
        }
        if(REST > 0){
            M <- frames[((NR*15)+1):(length(frames))]
            L <- paste(M, collapse=' ')
            write(L, file=filename, append=TRUE)
        }
    } else{
        M <- frames[1:(length(frames))]
        L <- paste(M, collapse=' ')
        write(L, file=filename, append=TRUE)
    }
}


