#' Title :Calculating the distances between pairs of chosen set  atoms for each frame
#'
#' @param pdb : a pdb structure object as returned by read.pdb or a numeric vector of ‘xyz’ coordinates.

#' @param inds : atom and xyz coordinate indices obtained from atom.select that selects the elements of pdb upon which the calculation should be based.
#' @param grp : logical, if TRUE atomic distances will be grouped according to their residue membership. See ‘grpby’
#' @param verbose
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dist_mat_frame <- function(pdb, inds = NULL, grp = TRUE, verbose=TRUE, ...)
{
  dm_output <- dm(pdb, inds = inds, grp = grp, verbose=verbose, ...)
  return(dm_output)

}
