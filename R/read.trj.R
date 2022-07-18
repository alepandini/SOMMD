#' @export
read.trj <- function(trjfile, topfile){
  trjfileExtension <- tools::file_ext(trjfile)
  topfileExtension <- tools::file_ext(trjfile)

  trj <- NULL
  trj$trjfile <- trjfile
  trj$trjformat <- trjfileExtension
  trj$topformat <- topfileExtension
  trj$topfile <- topfile
  trj$coord <- matrix(0)
  trj$top <- data.frame(0)
  trj$start <- c(0)
  trj$end<- c(0)
  trj$call <- ""

  return()
}
