#Package(bio3d)
library(bio3d)

#Read the pdb file
pdb <- read.pdb("./data/ref.pdb")
View(pdb)
class(pdb)
names(pdb)

#Structure of the file
str(pdb)

# extract residue numbers
residue_num <- pdb$atom[pdb$calpha, 'resno']
residue_num

# calculate distance matrix
dist_mat<- dm(pdb, inds=atom.select(pdb, 'protein'), mask.lower=FALSE)
dist_mat


#bwr.colors(n)
plot(dist_mat, color.palette=bwr.colors)

#mono.colors(n)
plot(dist_mat,
     resnum.1 = pdb$atom[pdb$calpha,"resno"],
     color.palette = mono.colors,
     xlab="Residue Number", ylab="Residue Number")

#head(distance_matrix)
#=======================================================
