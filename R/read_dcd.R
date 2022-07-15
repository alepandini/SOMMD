#' Read_dcd: Read CHARMM/X-PLOR/NAMD Binary DCD files
#'
#' @description
#' This is a wrap around \link[bio3d]{read.dcd} to standardized functions format
#' commonly used within this package and ease access to  function reading dcd
#' formatted file.
#'
#' the definition of argument duplicated from \link[bio3d]{read.dcd}:
#'
#'
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#' @param trjfile	 name of trajectory file to read. A vector if treat a batch of files
#' @param big	 logical, if TRUE attempt to read large files into a big.matrix object
#' @param verbose	 logical, if TRUE print details of the reading process.
#' @param cell	 logical, if TRUE return cell information only. Otherwise, return coordinates.
#'
#' @references
#'  \url{https://www.debian.org/doc/debian-policy/ch-controlfields.html}.
#'
#'  Note that R does not require encoding in UTF-8, which is a recent
#'   Debian requirement. Nor does it use the Debian-specific sub-format
#'   which allows comment lines starting with #.
#'
#' @return
#' @export
#'
#' @seealso \code{\link[SOMMD]{read_ncdf}, \link[SOMMD]{write_dcd},
#' \link[SOMMD]{read_pdb}, \link[SOMMD]{write_ncdf}}
#' @examples
#'
read_dcd <- function(trjfile,
                     big=FALSE,
                     verbose = TRUE,
                     cell = FALSE){



  fileExtension <- tools:: file_ext(trjfile)
  if (fileExtension == "nc"){
    stop("read_ncdf: to read file in netcdf format use read_ncdf or specify dcd format file")
  }
  if (fileExtension == "pdb"){
    stop("read_ncdf: to read file in pdb format use read_pdb or specify netcdf format file")
  }
  if (fileExtension != "dcd"){
    stop("read_ncdf: specified file is not in netcdf format file")
  }
  trajectories <- bio3d::read.dcd(trjfile = trjfile,
                                  big = big,
                                  verbose = verbose,
                                  cell = cell)


  return(trajectories)
}
