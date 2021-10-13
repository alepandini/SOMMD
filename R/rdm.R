#' Reduced size of distance matrix
#'
#' @description
#' this function only used calculated distance of between atoms which are
#' closer than cut.off radius to each other in reference frame
#'

#' @param pdb : a pdb structure object as returned by read.pdb or a numeric vector of ‘xyz’ coordinates.
#' @param cut.off : is cut.off radius and it is 10 Angstrom as default
#' @param inds : atom and xyz coordinate indices obtained from atom.select that selects the elements of pdb upon which the calculation should be based.
#' @param grp : logical, if TRUE atomic distances will be grouped according to their residue membership. See ‘grpby’
#' @param verbose :logical, if TRUE possible warnings are printed.
#' @param ... :arguments passed to and from functions.
#'
#'
#' @return
#' @export
#'
#'
#'
#'
"rdm" <- function(pdb, cut.off = 10,  inds = NULL, grp = TRUE, verbose=TRUE, ...)
{
  ref <- pdb
  ref$xyz <- pdb$xyz[1, ]
  ref_dm <- bio3d::dm(ref, inds = inds, grp = grp, verbose=verbose, ...)
  binary_dm <-  ref_dm < cut.off
  size <- sum(binary_dm , na.rm = TRUE) * (dim(pdb$xyz)[1])
  output_matrix <-  matrix(rep(0, size),
                           ncol = sum(binary_dm, na.rm = TRUE),
                           nrow = dim(pdb$xyz)[1])
  output_matrix[1, ]<- tidyr::drop_na (dplyr::tibble(ref_dm[binary_dm]))
  ##if (dim(pdb$xyz)[1] > 1){
     for ( i in 2:dim(pdb$xyz)[1] ) {
       ref$xyz <- pdb$xyz[i, ]
       ref_dm <-  bio3d::dm(ref)
       output_matrix[[i]] <- tidyr::drop_na (dplyr::as_tibble(ref_dm[binary_dm]))

    ## }
    }
  return(output_matrix)



}
