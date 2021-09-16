
#' Normalizer the Distance or similar matrices
#'
#' @param mat a given  numerical matrix
#' @param option the method of normalization iso or dimension-wise
#'
#' @return a normalized matrices with values between 0 and 1
#' @export
#'
#' @examples
#' not provided here yet
normalize_matrix <- function(mat, option = "iso") {

  # if ( typeof(mat) != double) {
  #   stop("the mat argument is not a numerical type, please assure providing a correct matrix")
  # }
  # if ( dim(mat)[1] <= 1 || dim(mat)[2] < 3) {
  #   warning("the provided martix is not presenting a coordinates of series of atoms in 3d space")
  # }

  if ("iso" ==  option) {
    iso_max <- max(mat)

    iso_min <- min(mat)

    normalMat <- (function(x, mi, ma) ((x-mi)/(ma - mi))) (mat, iso_min, iso_max)
    #normalMat <- lapply(mat , f(function(x, iso_min, iso_max) ((x-iso_min)/(iso_max - iso_min))))
  }
return (normalMat)
}


