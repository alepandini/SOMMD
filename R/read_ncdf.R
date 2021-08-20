#' Read AMBER Binary netCDF files
#'
#' @description
#'  This is a wrap around \link[bio3d]{read.nc} to standardized functions format
#' commonly used within this package and ease access to  function reading pdb
#' formatted file.
#'
#' the definition of argument duplicated from \link[bio3d]{read.nc}:
#'
#'
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#' @param trjfile name of trajectory file to read. A vector if treat a batch of files
#' @param headonly logical, if TRUE only trajectory header information is returned. If FALSE only trajectory coordinate data is return
#' @param verbose logical, if TRUE print details of the reading process.
#' @param time logical, if TRUE the first and last have the time unit ps; Otherwise the unit is the frame number.
#' @param first	 starting time or frame number to read; If NULL, start from the begining of the file(s).
#' @param last	 read data until last time or frame number; If NULL or equal to -1, read until the end of the file(s).
#' @param stride	 take at every stride frame(s)
#' @param cell	 logical, if TRUE and headonly is FALSE return cell information only. Otherwise, return header or coordinates.
#' @param at.sel	 an object of class ‘select’ indicating a subset of atomic coordinates to be read.
#'
#' @return
#' @export
#' @references
#' Grant, B.J. et al. (2006) Bioinformatics 22, 2695–2696.
#'  \url{https://www.unidata.ucar.edu/software/netcdf/ }
#'  \url{https://cirrus.ucsd.edu/~pierce/ncdf/ }
#'  \url{https://ambermd.org/FileFormats.php#netcdf}
#' @seealso \code{\link[SOMMD]{read_pdb}, \link[SOMMD]{write_nc}}
#'
#' @examples
#' nctrj <- read_ncdf("./data/TEST_001.nc")
read_ncdf <- function(trjfile,
                      headonly = FALSE,
                      verbose = TRUE,
                      time = FALSE,
                      first = NULL,
                      last = NULL,
                      stride = 1,
                      cell = FALSE,
                      at.sel = NULL){

  fileExtension <- tools:: file_ext(trjfile)
  if (fileExtension == "dcd"){
    stop("read_ncdf: to read file in dcd format use read_dcd or specify netcdf format file")
  }
  if (fileExtension == "pdb"){
    stop("read_ncdf: to read file in pdb format use read_pdb or specify netcdf format file")
  }
  if (fileExtension != "nc"){
    stop("read_ncdf: specified file is not in netcdf format file")
  }
  trajectories <- bio3d::read.ncdf(trjfile,
                                   headonly = headonly,
                                   verbose  = verbose,
                                   time     = time,
                                   first  = first,
                                   last   = last,
                                   stride = stride,
                                   cell   = cell,
                                   at.sel = at.sel)
  return(trajectories)

}
