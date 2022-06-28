#' Calculation of Distance matrix
#' @author Stefano Motta\email{stefano.motta@unimib.it}
#'
#' @param coord matrix of N atomic coordinates (N rows, 3 columns)
#'
#' @return MAT
#' @export
#'
#' @examples
#' TODO Not added yet

#Function to compute distance matrix given a set of coordinate (matrix)
Calc_Dist_Mat <- function(coord){
    MAT <- as.matrix(dist(coord), method='euclidean', upper=TRUE, diag=TRUE)
    return(MAT)
}
