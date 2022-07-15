#' @Title REQ 4: Merge pdb file and trajectories
#' @description Merging of a pdb file and a trajectory to get a pdb file as an output ,\link[bio3d]{read.pdb}
#'further a filter will be applied on this file to get the required file .
#'
#' @param pdb_file :pdb file
#' @param trj_file :traj file
#' @param rbind : binds both the files
#'
#' @author shaziya Ismail Mulla \email{shaziyaismail.m@@gmail.com} & Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#' @references : R Grant, B.J. et al. (2006) Bioinformatics 22, 2695–2696.
#'
#'For a description of PDB format (version3.3) see:
#' \url{http://www.wwpdb.org/documentation/format33/v3.3.html}.
#'
#' @seealso \code{\link[SOMMD]{read_ncdf},
#' \link[SOMMD]{read_pdb},
#'
#' @return
#' @export
#'
#' @examples :

Merge_pdb_trj <- function(pdb_file, trj_file){

  if(dim(pdb_file$xyz)[2]==dim(trj_file)[2]){

output_merge <- rbind(pdb_file$xyz , trj_file)
pdb <- pdb_file

pdb$xyz <- output_merge
return(pdb)
}
else {

  stop("Both the pdb and trj files dimension dont match")
}
  }
