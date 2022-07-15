#' Title :Calculating the distances between pairs of chosen set  atoms for each frame
#' '@description
#' This is a wrap around \link[bio3d]{dm}
#' Construct a distance matrix for a given protein structure.Returns a numeric matrix of class "dmat",
#' with all N by N distances, where N is the number of
#' selected atoms. With multiple frames the output is provided in a three dimensional array.
#' the definition of argument duplicated from \link[bio3d]{dm}:
#'
#'
#' @author Shaziya Ismail Mulla\email{shaziyaismail.m@@gmail.com}
#'
#'
#'
#'
#' @param pdb : a pdb structure object as returned by read.pdb or a numeric vector of ‘xyz’ coordinates.
#' @param inds : atom and xyz coordinate indices obtained from atom.select that selects the elements of pdb upon which the calculation should be based.
#' @param grp : logical, if TRUE atomic distances will be grouped according to their residue membership. See ‘grpby’
#' @param verbose :logical, if TRUE possible warnings are printed.
#' @param ... :arguments passed to and from functions.
#'
#'
#'
#' @references : https://cran.r-project.org/web/packages/bio3d/bio3d.pdf
#'               https://search.r-project.org/CRAN/refmans/bio3d/html/dm.html
#'               https://rdrr.io/cran/bio3d/man/dm.html
#'
#'
#' @return
#' @export
#' @seealso \code{\link[SOMMD]{dm}
#'
#' @examples

#'
dist_mat_frame <- function(pdb, inds = NULL, grp = TRUE, verbose=TRUE, ...)
{
  dm_output <- bio3d::dm(pdb, inds = inds, grp = grp, verbose=verbose, ...)
  return(dm_output)

}
