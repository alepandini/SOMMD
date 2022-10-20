#' Title
#'
#' @param cm
#' @param cutoff.sims
#'
#' @return
#' @export
#'
#' @examples
Filter<- function(cm, cutoff.sims = NULL)
{
  filter_output <- bio3d::filter(cm=cm, cutoff.sims = cutoff.sims)
  return(filter_output)

}
