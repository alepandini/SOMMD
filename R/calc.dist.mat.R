#' @title Calculation of Distance matrix
#' @description Compute the pairwise distance matrix of a given set of coordinates
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#' @param coord matrix of N atomic coordinates (N rows, 3 columns)
#' @return The pairwise distance matrix
#' @noRd
#'
calc.dist.mat <- function(coord){
    mat <- as.matrix(stats::dist(coord), method='euclidean', upper=TRUE, diag=TRUE)
    return(mat)
}
