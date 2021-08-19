#' @name read_pdb
#' @title Read PDB files
#'
#' @description
#' This is a wrap around \link[bio3d]{read.pdb} to standardized functions format
#' commonly used within this package and ease access to  function reading pdb
#' formatted file.
#'
#'
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#'
#'
#' the definition of argument duplicated from \link[bio3d]{read.pdb}:
#'
#' @param file   a single element character vector containing the name of the PDB file to be read, or the four letter PDB identifier for online file access.
#' @param maxlines the maximum number of lines to read before giving up with large files. By default if will read up to the end of input on the connection.
#' @param multi    logical, if TRUE multiple ATOM records are read for all models in multi-model files and their coordinates returned.
#' @param rm.insert   logical, if TRUE PDB insert records are ignored.
#' @param rm.alt  logical, if TRUE PDB alternate records are ignored.
#' @param ATOM.only   logical, if TRUE only ATOM/HETATM records are stored. Useful for speed enhancements with large files where secondary structure, biological unit and other remark records are not required.
#' @param hex      logical, if TRUE enable parsing of hexadecimal atom numbers (> 99.999) and residue numbers (> 9.999) (e.g. from VMD). Note that numbering is assumed to be consecutive (with no missing numbers) and the hexadecimals should start at atom number 100.000 and residue number 10.000 and proceed to the end of file.
#' @param verbose   print details of the reading process.
#'
#'
#'@references Grant, B.J. et al. (2006) Bioinformatics 22, 2695–2696.
#'
#'For a description of PDB format (version3.3) see:
#' \url{http://www.wwpdb.org/documentation/format33/v3.3.html}.
#'
#' @seealso \code{\link[SOMMD]{read_traj}}
#'
#'

function(file, maxlines = -1, multi = FALSE, rm.insert = FALSE,
         rm.alt = TRUE, ATOM.only = FALSE, hex = FALSE, verbose = TRUE){

  if(tools::file_ext(file) != "pdb" | length(file) == 4 ){
    stop("read_pdb: the specified file is not in pdb format or pdb index")
  }
  pdb <- bio3d::read.pdb(file, maxlines = maxlines, multi = multi  ,
                  rm.insert = rm.insert,
                  rm.alt = rm.alt,
                  ATOM.only  = ATOM.only,
                  hex = hex,
                  verbose = verbose)
  return(pdb)
}
