#' @title TBC
#' @description A short description...
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param x trj object
#' @param ... additional arguments to be passed to further methods
#' @export
#'
print.struct <- function(x, ...) {
  # Print a summary of basic struct object features
  struct <- x
  y <- summary.struct(struct)
}
