#' @title Compute euclidean distace
#' @description Function to compute euclidean distance between two vectors
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param V1 the first vector
#' @param V2 the second vector
#' @return The euclidean distance between the two vectors
#' @noRd
#'
compute.distance <- function(V1, V2){
    return(as.numeric(stats::dist(rbind(V1, V2))))
}

#' @title List two letters
#' @description Function to create a vector of two letters identifiers
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param letters the vector of the possible letters to combine
#' @return A vector of the possible combination of two letters
#' @noRd
#'
TwoLetters <- function(letters=LETTERS){
    LET <- NULL
    #Create a combination of two letters
    for(i in letters){
        for(j in letters){
        LET <- c(LET, paste(i, j, sep=''))
        }
    }
    return(LET)
}
