#' @title Summarizing a structure object
#' @description summary method for class struct
#' @author Stefano Motta \email{stefano.motta@unimib.it}
#' @param object struct object
#' @param ... additional arguments to be passed to further methods
#' @export
#'
summary.struct <- function(object, ...) {
  if(!methods::is(object,"struct")){
    stop("Input should be a struct object, as obtained from 'read.struct()'")
  }
  nxyz <- length(object$xyz[1,])
  ntotal <- nxyz/3
  resno <- as.numeric(as.character(object$atom$resno))
  resid <- as.numeric(as.factor(object$atom$resid))
  resno_change <- c(TRUE, diff(resno) != 0)
  resid_change <- c(TRUE, diff(resid) != 0)
  residue_change <- resno_change | resid_change
  new_residue_lines <- which(residue_change)
  nres <- length(new_residue_lines)
  prot.res <- c("ALA", "CYS", "ASP", "GLU", "PHE", "GLY", "HIS", "ILE", "LYS", "LEU", "MET", "ASN", "PRO", "GLN", "ARG", "SER", "THR", "VAL", "TRP", "TYR")
  nucl.res <- c("A",   "U",  "G",  "C", "T",  "I", "DA", "DU", "DG", "DC",  "DT", "DI")
  all.inds <- c(1: nrow(object$atom))
  prot.inds <- which( object$atom$resid %in% prot.res )
  nucl.inds <- which( object$atom$resid %in% nucl.res )
  other.inds <-  all.inds[! (all.inds %in% c(prot.inds, nucl.inds)) ]
  nres.prot <- length( which( object$atom$resid[new_residue_lines] %in% prot.res ))
  nres.nucl <- length( which( object$atom$resid[new_residue_lines] %in% nucl.res ))
  nres.other <- nres-(nres.prot + nres.nucl)
  chains <- unique(object$atom[,"chain"])
  chains[which(is.na(chains))] <- "' '"

  if(length(other.inds) == 0){
    s.hetres <- ""
  } else{
    s.hetres <- paste("\n     Non-protein/nucleic resid values: [ ", unique(object$atom$resid[other.inds])," ]")
  }

  cat("\n Call:  ", paste(deparse(object$call), sep = "\n", collapse = "\n"),
        "\n", sep = "")

  s <- paste0("\n     Total Atoms#: ", ntotal, ",  XYZs#: ", nxyz,
  			  "  Chains#: ", length(chains),
  			  "  (values: ", paste(chains, collapse=" "),")",

             "\n\n     Protein Atoms#: ", length(prot.inds),
              "  (residues#: ", nres.prot,")",

              "\n     Nucleic acid Atoms#: ", length(nucl.inds),
              "  (residues#: ", nres.nucl,")",

             "\n\n     Non-protein/nucleic Atoms#: ", length(other.inds),
             "  (residues#: ", nres.other, ")",
             s.hetres,
             "\n\n")

  cat(s)

  i <- paste( attributes(object)$names, collapse=", ")
  cat(strwrap(paste(" + attr:",i,"\n"),width=45, exdent=8), sep="\n")

}
