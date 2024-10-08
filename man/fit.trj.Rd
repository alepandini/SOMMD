% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.trj.R
\name{fit.trj}
\alias{fit.trj}
\title{Coordinate superposition}
\usage{
fit.trj(trj, ref = NULL, trj.inds = NULL, ref.inds = NULL)
}
\arguments{
\item{trj}{an object with class trj}

\item{ref}{a struct object read with read.struct() to be used as reference}

\item{trj.inds}{a vector of indices that selects the trj atoms upon which fitting should be based. If not specified all atoms will be used.}

\item{ref.inds}{a vector of indices that selects the ref atoms upon which fitting should be based. If not specified all atoms will be used.}
}
\value{
A trj object aligned
}
\description{
Coordinate superposition with the Kabsch algorithm. This function make use of the bio3d fit.xyz function to align a SOMMD trj object. If ref is not specified, the trj object is aligned to the first frame of the simulation, otherwise it is aligned to the reference input object.
}
\examples{
#Read trajectory
trj <- read.trj(trjfile = system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD"),
  topfile = system.file("extdata", "HIF2a.gro", package = "SOMMD"))
# Fit a trajectory to the first frame based on alpha carbons:
ca.inds <- which(trj$top$elety=="CA")
trj.fit <- fit.trj(trj, trj.inds=ca.inds)

}
\author{
Stefano Motta \email{stefano.motta@unimib.it}
}
