#' Title
#'
#' @param VEC
#'
#' @return
#' @export
#'
#' @examples
Calc_Dist_Mat <- function(VEC){
  N_atm <- (length(VEC)-1)/3
  MAT <- as.matrix((dist(t(matrix(VEC[2:length(VEC)], ncol=N_atm)), method='euclidean', upper=TRUE, diag=TRUE)))
  return(VEC)
}
