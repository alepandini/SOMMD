#' Writing a coordinate in netCDF format
#'
#' @description
#'  This is a wrap around \link[bio3d]{write.ncdf} to standardized functions format
#' commonly used within this package and ease access to  function reading netCDF
#' formatted file.
#'
#' the definition of argument duplicated from \link[bio3d]{read.ncdf}:
#'
#'
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#' @param  x	 A numeric matrix of xyz coordinates with a frame/structure per row and a Cartesian coordinate per column.
#' @param trjfile	 name of the output trajectory file.
#' @param cell	 A numeric matrix of cell information with a frame/structure per row and a cell length or angle per column. If NULL cell will not be written.
#' @return None
#' @export
#'
#' @references  Grant, B.J. et al. (2006) Bioinformatics 22, 2695–2696.
#'  \url{https://www.unidata.ucar.edu/software/netcdf/ }
#'  \url{https://cirrus.ucsd.edu/~pierce/ncdf/ }
#'  \url{https://ambermd.org/FileFormats.php#netcdf}
#'
#' @seealso \code{\link[SOMMD]{read_pdb}, \link[SOMMD]{write_ncdf}}
#'
#'
write_ncdf <- function(x, trjfile = "R.ncdf", cell = NULL){
  write.ncdf(x = x, trjfile = trjfile, cell = cell)
}
