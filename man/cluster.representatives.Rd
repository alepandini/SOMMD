% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.representatives.R
\name{cluster.representatives}
\alias{cluster.representatives}
\title{Cluster Representatives}
\usage{
cluster.representatives(SOM, clusters)
}
\arguments{
\item{SOM}{a kohonen SOM object.}

\item{clusters}{a vector of clusters assignment for each neuron, as returned for example by hclust.}
}
\value{
A vector of frames representatives of each neuron
}
\description{
Compute the cluster representatives
}
\examples{
#Read example SOM data
som_model <- readRDS(system.file("extdata", "SOM_HIFa.rds", package = "SOMMD"))
# Divide the SOM in the selected number of clusters
som_cl <- cutree(hclust(dist(som_model$codes[[1]], method="euclidean"), method="complete"), 4)
#Get representative frames for each cluster
cl_repres <- cluster.representatives(som_model, som_cl)

}
\author{
Stefano Motta \email{stefano.motta@unimib.it}
}
