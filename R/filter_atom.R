#' Filter pdb file by particular atoms
#' @description
#' the function accept a pdb file and filter a given atom, and return either a pdb file or only coordinates
#'
#'  @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#' @param pdb_file  a \code{bio3d} pdb object
#' @param atom      the atom which pdb file should be filtered
#' @param output_type the output of the function  can be either pdb object which
#'   only contain attributes of the atoms, or a trajectory file contain the atom coordinates
#'
#' @return
#' @export
#'
#' @examples
#' pdb1 <- read_pdb("../SOMMD/data/ref_1782.pdb")
#' trj1 <- read_ncdf("../SOMMD/data/TEST_001.nc")
#' merged_pdb <-  merge_pdb_trj(pdb1 , trj1)
#' ## pdb outputs
#' cb_pdb <-  filter_atoms(merged_pdb , atom = "CB", output_type = "pdb")
#'
#' ## coordinates output
#' cb_coordinates <-  filter_atoms(merged_pdb , atom = "CB", output_type = "xyz")
"filter_atoms" <- function(pdb_file , atom = "CA", output_type = "pdb") {

  ## Check the input files
  if(class(pdb_file)[1] != "pdb") {
    stop(crayon::yellow("the input does not from pdb format"))
  }
  ## check the atoms can be find in the atoms
  if ( !(atom %in% unique(pdb_file$atom$elety)) ) {
   stop(crayon::yellow("the chosen atom does not exist in input protein"))
  }
  ## Check the requested output
  if(!(output_type %in% c("pdb" , "xyz"))) {
    stop(crayon::yellow("the output can only be pdb object or coordinate matrix"))
  }
  ##define the parameters and list needed in the funciton
  number_of_atoms <- length(pdb_file$atom$elety)
  atom3 <-  rep("A" , number_of_atoms * 3 )

  ## a loop to expand atoms to coordinate length which Enable to filter the coordinate
  for (i in seq(1,number_of_atoms*3)){


    atom3[i] <-  pdb_file$atom$elety[ceiling(i/3)]
  }
  ## filter coordinates of atoms needed
  chosen_atoms <- ifelse(atom3 == "CA", T, F)
  coordinate_of_chosen_atoms <-  pdb_file$xyz[,chosen_atoms]
  ## tailoring the output as requested
  ## if output needs to be  pdb file but only contain information about the chosen atoms
  if ( output_type == "pdb") {
    mini_pdb <- pdb_file
    mini_pdb$atom <- pdb_file$atom[pdb_file$atom$elety == "CA" , ]
    mini_pdb$calpha <- pdb_file$calpha[ pdb_file$atom$elety == "CA"]
    return(mini_pdb)
    ## if the output needs to be just the coordinate of chosen atoms
  }else if(output_type == "xyz") {
    return(coordinate_of_chosen_atoms)
  }
}


