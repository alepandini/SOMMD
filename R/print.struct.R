#' @title print.struct
#' @description A short description...
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param x trj object
#' @param ... additional arguments to be passed to further methods
#' @return Called for its effect.
#' @export
#' @examples
#' # Read structure file
#' struct <- read.struct(system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' #Print basic information
#' print(struct)
print.struct <- function(x, ...) {
  # Print a summary of basic struct object features
  struct <- x
  y <- summary.struct(struct)
}
