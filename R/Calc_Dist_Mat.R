#' Calculation of Distance matrix
#' @author Shaziya Ismail Mulla\email{shaziyaismail.m@@gmail.com}
#'
#' @param VEC vector
#' @param N_atm the number of atom
#' @param dist the function distance
#'
#' @return MAT
#' @export
#'
#' @examples
Calc_Dist_Mat <- function(VEC){
  N_atm <- (length(VEC)-1)/3
  MAT <- as.matrix((dist(t(matrix(VEC[2:length(VEC)], ncol=N_atm)), method='euclidean', upper=TRUE, diag=TRUE)))
  return(MAT)
}
