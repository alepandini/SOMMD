#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @export
#'

print.trj <- function(x, ...) {
  # Print a summary of basic trj object features
  trj <- x
  y <- summary.trj(trj)
}
