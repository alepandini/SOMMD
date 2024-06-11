#' @author Stefano Motta \email{stefano.motta@unimib.it}
#'
#' @export
#'

print.struct <- function(x, ...) {
  # Print a summary of basic struct object features
  struct <- x
  y <- summary.struct(struct)
}
