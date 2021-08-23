#' Pre Align the Trajectory
#'
#'@description
#'This is a wrap around \link[bio3d]{pdbaln}
#'This Creates multiple sequences alignments from a list of PDB files returning aligned sequence and structure records
#'This is written for re-aligns the trajectory,  using output of requirement (REQ-2) as input ,
#' Returns a list of class "pdbs" with the five  components: xyz, resno, b, chain, id, ali, and call. The coordinates of the aligned trajectory.
#'  the definition of argument duplicated from \link[bio3d]{pdbaln}:
#'
#'
#'  @author Shaziya Ismail Mulla\email{shaziyaismail.m@@gmail.com}
#'
#' @param files a character vector of PDB file names. Alternatively, a list of pdb objects can be provided.
#' @param fit fit logical, if TRUE coordinate superposition is performed on the input structures.
#' @param pqr pqr logical, if TRUE the input structures are assumed to be in PQR format.
#' @param ncore ncore number of CPU cores used to do the calculation. ncore>1 requires package ‘parallel’ installed.
#' @param nseg.scale nseg.scale split input data into specified number of segments prior to running multiple core calculation. See fit.xyz.
#' @param progress progress progress bar for use with shiny web app.
#' @param ...... extra arguments passed to seqaln function.

#' @return
#' @export
#' @seealso \code{\link[SOMMD]{pdbaln}}
#'
#' @examples
#' not yet
Pre_algn_pdbaln <- function(files, fit = FALSE, pqr = FALSE, ncore = 1, nseg.scale = 1, progress = NULL, ...){
  output <-pdbaln(files, fit = fit , pqr = pqr, ncore = ncore, nseg.scale = nseg.scale, progress = progress, ...)
  return(output)
}



