get_distance_matrix <- function(pdb){
  pdb <- read.pdb("C:/Users/shazi/Documents/Dissertation/Git/Protein/ref.pdb")
  residue_num <- pdb$atom[pdb$calpha, 'resno']
  dist_mat<- dm(pdb, inds=atom.select(pdb, 'protein'), mask.lower=FALSE)
  plot(dist_mat, color.palette=bwr.colors)
  return(dist_mat)
}

get_distance_matrix

