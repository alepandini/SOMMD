#' Calculation of Distance matrix
#'
#' Compute the pairwise distance matrix of a given set of coordinates
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param coord matrix of N atomic coordinates (N rows, 3 columns)
#'
#' @return mat
#' @export
#'

calc.dist.mat <- function(coord){
    mat <- as.matrix(dist(coord), method='euclidean', upper=TRUE, diag=TRUE)
    return(mat)
}
