#' @title TBC
#' @description A short description...
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param x trj object
#' @param ... additional arguments to be passed to further methods
#' @export
#'
print.trj <- function(x, ...) {
  # Print a summary of basic trj object features
  trj <- x
  y <- summary.trj(trj)
}
