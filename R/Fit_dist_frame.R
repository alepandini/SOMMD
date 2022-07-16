#' Title :REQ3 : Pre-aligns the trajectory,  using output of requirement (REQ-2) as input
#'@description
#'Coordinate superposition with the Kabsch algorithm.
#'The function fit.xyz is a wrapper for the function rot.lsq, which performs the actual coordinate
#'superposition. The function rot.lsq is an implementation of the Kabsch algorithm (Kabsch, 1978)
#'and evaluates the optimal rotation matrix to minimize the RMSD between two structures.

#'
#'
#' @param fixed :fixed numeric vector of xyz coordinates.
#' @param mobile :mobile numeric vector, numeric matrix, or an object with an xyz component containing one or more coordinate sets.
#' @param fixed.inds:fixed.inds a vector of indices that selects the elements of fixed upon which fitting should
#' be based.
#' @param mobile.inds :mobile.inds a vector of indices that selects the elements of mobile upon which fitting should
#'be based.
#' @param verbose :logical, if TRUE more details are printed.
#' @param prefix :prefix to mobile$id to locate “full” input PDB files. Only required if full.pdbs
#' is TRUE.
#' @param pdbext :the file name extension of the input PDB files
#' @param outpath :character string specifing the output directory when full.pdbs is TRUE
#' @param full.pdbs :full.pdbs logical, if TRUE “full” coordinate files (i.e. all atoms) are written to the location
#'specified by outpath.
#'@param prefix :prefix to mobile$id to locate “full” input PDB files. Only required if full.pdbs
#'is TRUE.
#' @param ncore :number of CPU cores used to do the calculation. ncore>1 requires package
#' ‘parallel’ installed.
#' @param nseg.scale :split input data into specified number of segments prior to running multiple core
#'calculation.
#' @param ...other parameters for read.pdb.
#'
#'
#'
#' @author shaziya Ismail Mulla \email{shaziyaismail.m@@gmail.com}
#'
#'
#'
#' @references Grant, B.J. et al. (2006) Bioinformatics 22, 2695–2696.
#' @references :https://www.rdocumentation.org/packages/bio3d/versions/2.4-2/topics/pdbaln
#'              https://rdrr.io/cran/bio3d/man/pdbaln.html
#'              https://search.r-project.org/CRAN/refmans/bio3d/html/pdbaln.html
#'
#' @seealso \code{\link[SOMMD]{fit.xyz}}
#'
#' @return
#' @export
#'
#' @examples
fit_dist_frame<- function(fixed, mobile,
                                   fixed.inds = NULL,
                                   mobile.inds = NULL,
                                   verbose=FALSE,
                                   prefix= "", pdbext = "",
                                   outpath = "fitlsq", full.pdbs=FALSE,
                                   ncore = 1, nseg.scale = 1, ...)
{
  fit_output <- bio3d::fit.xyz(fixed=fixed, mobile=mobile,
                         fixed.inds = fixed.inds,
                         mobile.inds = mobile.inds,
                         verbose=verbose,
                         prefix= "", pdbext = "",
                         outpath = "fitlsq", full.pdbs=full.pdbs,
                         ncore = 1, nseg.scale = 1, ..)
  return(fit_output)

}
