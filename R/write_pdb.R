#' Write PDB Format Coordinate File
#'
#' @description
#'  This is a wrap around \link[bio3d]{read.ncdf} to standardized functions format
#' commonly used within this package and ease access to  function reading netCDF
#' formatted file.
#'
#'
#'
#' Write a Protein Data Bank (PDB) file for a given ‘xyz’ Cartesian coordinate vector or matrix.
#'
#' the definition of argument duplicated from \link[bio3d]{read.ncdf}:
#'
#'
#'  @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#'
#' @param pdb 	  a PDB structure object obtained from read.pdb.
#' @param file 	  the output file name.
#' @param xyz 	  Cartesian coordinates as a vector or 3xN matrix.
#' @param type 	  vector of record types, i.e. "ATOM" or "HETATM", with length equal to length(xyz)/3.
#' @param resno 	  vector of residue numbers of length equal to length(xyz)/3.
#' @param resid 	  vector of residue types/ids of length equal to length(xyz)/3.
#' @param eleno 	  vector of element/atom numbers of length equal to length(xyz)/3.
#' @param elety 	  vector of element/atom types of length equal to length(xyz)/3.
#' @param chain 	  vector of chain identifiers with length equal to length(xyz)/3.
#' @param insert 	  vector of insertion code with length equal to length(xyz)/3.
#' @param alt 	  vector of alternate record with length equal to length(xyz)/3.
#' @param o 	  vector of occupancy values of length equal to length(xyz)/3.
#' @param b 	  vector of B-factors of length equal to length(xyz)/3.
#' @param segid 	  vector of segment id of length equal to length(xyz)/3.
#' @param elesy 	  vector of element symbol of length equal to length(xyz)/3.
#' @param charge 	  vector of atomic charge of length equal to length(xyz)/3.
#' @param append 	  logical, if TRUE output is appended to the bottom of an existing file (used primarly for writing multi-model files).
#' @param verbose 	  logical, if TRUE progress details are printed.
#' @param chainter 	  logical, if TRUE a TER line is inserted at termination of a chain.
#' @param end 	  logical, if TRUE END line is written.
#' @param sse 	  logical, if TRUE secondary structure annotations are written.
#' @param print.segid 	  logical, if FALSE segid will not be written.
#'
#' @details
#' Writes an AMBER netCDF (Network Common Data Form) format trajectory file with the help of David W. Pierce's (UCSD) ncdf4 package available from CRAN.
#'
#' @note
#' See AMBER documentation for netCDF format description.
#'
#'
#' NetCDF binary trajectory files are supported by the AMBER modules sander, pmemd and ptraj. Compared to formatted trajectory files, the binary trajectory files are smaller, higher precision and significantly faster to read and write.
#'
#' NetCDF provides for file portability across architectures, allows for backwards compatible extensibility of the format and enables the files to be self-describing. Support for this format is available in VMD.
#'
#'
#'  @references  Grant, B.J. et al. (2006) Bioinformatics 22, 2695–2696.
#'  \url{https://www.unidata.ucar.edu/software/netcdf/ }
#'  \url{https://cirrus.ucsd.edu/~pierce/ncdf/ }
#'  \url{https://ambermd.org/FileFormats.php#netcdf}
#'
#'
#' @return None
#' @export
#'
#' @seealso \code{\link[SOMMD]{read_pdb}, \link[SOMMD]{write_ncdf}}
#' @examples
#'
#'
write_pdb <- function(pdb = NULL, file = "R.pdb", xyz = pdb$xyz, type = NULL, resno = NULL,
                      resid = NULL, eleno = NULL, elety = NULL, chain = NULL, insert = NULL,
                      alt = NULL, o = NULL, b = NULL, segid = NULL, elesy = NULL, charge = NULL,
                      append = FALSE, verbose = FALSE, chainter = FALSE, end = TRUE, sse = FALSE,
                      print.segid = FALSE){

  write.pdb(pdb = pdb, file = file, xyz = xyz, type = type, resno = resno,
            resid = resid, eleno = eleno, elety = elety, chain = chain, insert = insert,
            alt = alt, o = o, b = b, segid = segid, elesy = elesy, charge = charge,
            append = append, verbose = verbose, chainter = chainter, end = end, sse = sse,
            print.segid = print.segid)
}
