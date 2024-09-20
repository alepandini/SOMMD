#' @useDynLib SOMMD, .registration=TRUE
NULL

#' @title Read xtc trajectory file
#' @description Function to read a xtc trajectory file
#' @author Alessandro Pandini
#' @param xtc_filename contains the name and the path to the xtc file
#' @return Returns number of atoms in the structure
rio_read_xtc_natoms <- function(xtc_filename){
  natms <- .Call("rio_read_xtc_natoms_", xtc_filename)
  return(natms)
}

#' @title Read xtc trajectory file
#' @description Function to read an xtc trajectory file
#' @author Alessandro Pandini
#' @param xtc_filename contains the name and the path to the xtc file
#' @return Returns number of frames in the trajectory
rio_read_xtc_nframes <- function(xtc_filename){
  nframes <- .Call("rio_read_xtc_nframes_", xtc_filename)
  return(nframes)
}

#' @title Read xtc trajectory file
#' @description Function to read an xtc trajectory file
#' @author Alessandro Pandini
#' @param xtc_filename contains the name and the path to the xtc file
#' @return Returns 3D array of cartesian coordinates
rio_read_xtc <- function(xtc_filename){
  xyz_3D_array <- .Call("rio_read_xtc_", xtc_filename)
  return(xyz_3D_array)
}

rio_coord_reshape <- function(xyz_3D_array){
  array_dim <- dim(xyz_3D_array)
  perm_xyz_array <- aperm(xyz_3D_array, c(2,1,3))
  dim(perm_xyz_array) <- c(array_dim[1] * array_dim[2], array_dim[3])
  reshaped_xyz_array <- aperm(perm_xyz_array, c(2,1))
  return(reshaped_xyz_array)
}

#' @title Read xtc trajectory file
#' @description Function to read an xtc trajectory file
#' @author Alessandro Pandini
#' @param xtc_filename contains the name and the path to the xtc file
#' @return Returns bio3d xyz array of Cartesian coordinates
rio_read_xtc2xyz <- function(xtc_filename){
  xyz_3D_array <- rio_read_xtc(xtc_filename)
  reshaped_xyz_array <- rio_coord_reshape(xyz_3D_array)
  return(bio3d::as.xyz(reshaped_xyz_array))
}

#' @title Write xtc trajectory file
#' @description Function to write an xtc trajectory file
#' @author Alessandro Pandini
#' @param xtc_filename contains the name and the path to the xtc file to write
#' @param trj trajectory object to save
#' @return Returns status of write execution
rio_write_xtc <- function(xtc_filename, trj){
  coords <- trj$coord
  natoms <- dim(coords)[1]
  nframes <- dim(coords)[3]
  status <- .Call("rio_write_xtc_", xtc_filename, coords, natoms, nframes)
  return(status)
}
