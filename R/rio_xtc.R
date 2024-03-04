#' @useDynLib SOMMD, .registration=TRUE
#' @export

rio_read_xtc_natoms <- function(xtc_filename){
  natms <- .Call("rio_read_xtc_natoms_", xtc_filename)
  return(natms)
}

#' @export
rio_read_xtc_nframes <- function(xtc_filename){
  nframes <- .Call("rio_read_xtc_nframes_", xtc_filename)
  return(nframes)
}

#' @export
rio_read_xtc <- function(xtc_filename){
  xyz_3D_array <- .Call("rio_read_xtc_", xtc_filename)
  return(xyz_3D_array)
}

#' @export
rio_coord_reshape <- function(xyz_3D_array){
  array_dim <- dim(xyz_3D_array)
  perm_xyz_array <- aperm(xyz_3D_array, c(2,1,3))
  dim(perm_xyz_array) <- c(array_dim[1] * array_dim[2], array_dim[3])
  reshaped_xyz_array <- aperm(perm_xyz_array, c(2,1))
  return(reshaped_xyz_array)
}

#' @export
rio_read_xtc2xyz <- function(xtc_filename){
  xyz_3D_array <- rio_read_xtc(xtc_filename)
  reshaped_xyz_array <- rio_coord_reshape(xyz_3D_array)
  return(bio3d::as.xyz(reshaped_xyz_array))
}

#' @export
rio_write_xtc <- function(xtc_filename){
  status <- .Call("rio_write_xtc_", xtc_filename)
  return(status)
}
