read.trj <- function(trj_filename){
  fileExtension <- tools::file_ext(trj_filename)
  return(fileExtension)
}
