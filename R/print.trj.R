#' @title Print Trajectory
#' @description A short description...
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param x trj object
#' @param ... additional arguments to be passed to further methods
#' @return Called for its effect.
#' @export
#' @examples
#' #Read trajectory
#' trj <- read.trj(trjfile = system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD"),
#'   topfile = system.file("extdata", "HIF2a.gro", package = "SOMMD"))
#' #Print basic informations
#' print(trj)
print.trj <- function(x, ...) {
  # Print a summary of basic trj object features
  trj <- x
  y <- summary.trj(trj)
}
