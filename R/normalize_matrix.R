
#' Scaling the Distance or similar matrices by standardization or normalization
#'i @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#' @param method accepts a character string determines what method implemented
#'   *minmax* = transform the range to 0 and
#'   "robust" = ?
#'   "maxabs" = transform to a range of [-1 , 1],
#'   "standard" = normalize the distribution with zero-mean and unit-variance
#' @param by
#' _feature_ scale each feature of matrices independently
#' _trajaxis_ scale each axis of 3d space independently
#' _whole_ applying scaling method to the whole matrix
#' @param mat a given  numerical matrix
#'
#' @return a normalized matrices with values between 0 and 1
#' @export
#'
#' @examples
#' not provided here yet
scaler <- function(mat, method, by = "feature") {

  output <- switch (by,
    "feature" = feature(mat, method),
    "whole" = scaling (mat , method),
    "trajaxis" = axis(mat , method)
  )
  return(output)
}

feature <- function (mat , method) {

  output <-  apply(mat, 2, function(x) scaling(x, method))
  return(output)
}

axis <- function( mat , method){
    ncols <- ncol(mat)
    x_ind <- seq(1, ncols, 3)
    y_ind <- seq(2, ncols, 3)
    z_ind <- seq(3, ncols, 3)

    axis_x <- mat[,x_ind]
    axis_y <- mat[,y_ind]
    axis_z <- mat[,z_ind]

    scaled_x <- scaling (axis_x, method)
    scaled_y <- scaling (axis_y, method)
    scaled_z <- scaling (axis_z, method)

    output[,x_ind] <- scaled_x
    output[,y_ind] <- scaled_y
    output[,z_ind] <- scaled_z
}


scaling <- function(mat , method){
  switch (method,
    "minmax" = minmax(mat),
    "robust" = robust(mat),
    "maxabs" = maxabs(mat),
    "standard" = standard(mat)

    )
  }



minmax <- function(mat){
  min_dat <- min(mat)
  max_dat <- max(mat)
  minmax_output <- (mat - min_dat) / (max_dat - min_dat)

  return(minmax_output)
}

standard <- function(mat) {
  mean_dat <- mean(mat)
  std_dat <- st(mat)
  standard_output <-  (mat - mean_dat) / st
  return (standard_output)
}

maxabs <- function(mat) {
  abs_maximum <- max(abs(mat))
  maxabs_output <- mat / abs_maximum
  return(maxabs_output)
}





