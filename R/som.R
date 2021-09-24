#' Self Organizing Map function
#'
#' @description
#' The function is a wrapper over \link[kohonen]{supersom} to cover all the
#' necessary function within the package, besides more specialized example for
#' clustering MD trajectories.
#'
#'
#' the definition of argument duplicated from \link[kohonen]{supersom}:
#'
#'
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#'
#'
#' @param data list of data matrices (numerical) of factors. If a vector is entered,
#'  it will be converted to a one-column matrix. No
#'  \code{data.frame} objectss are allowed.
#'
#' @param grid a grid for the codebook vectors: see \code{somgrid}.
#' @param rlen the number of times the complete data set will be presented to the network.
#' @param alpha learning rate, a vector of two numbers indicating the amount of change.
#'  Default is to decline linearly from 0.05 to 0.01 over \code{rlen} updates.
#'  Not used for the batch algorithm.
#' @param radius the radius of the neighbourhood, either given as a
#' single number or a vector (start, stop). If it is given as a single number
#'  the radius will change linearly from \code{radius} to zero; as
#'  soon as the neighbourhood gets smaller than one only the winning unit
#'   will be updated. Note that the default before version 3.0 was to run
#'    from \code{radius} to \code{-radius}. If nothing is supplied, the
#'     default is to start with a value that covers 2/3 of all unit-to-unit
#'      distances.
#' @param whatmap What data layers to use. If unspecified all layers are used.
#' @param user.weights the weights given to individual layers. This can
#' be a single number (all layers have the same weight, the default), a
#' vector of the same length as the \code{whatmap} argument, or a vector
#' of the same length as the \code{data} argument. In xyf maps, this
#' argument provides the same functionality as the now-deprecated
#' \code{xweight} argument that was used prior to version 3.0.
#'
#' @param maxNA.fraction the maximal fraction of values that may be NA to
#'  prevent the row to be removed.
#' @param keep.data if TRUE, return original data and mapping
#' information. If FALSE, only return the trained map (in essence the codebook vectors).
#' @param dist.fcts vector of distance functions to be used for the
#' individual data layers, of the same length as the \code{data}
#' argument, or the same length of the \code{whatmap} argument. If the
#' length of this vector is one, the
#' same distance will be used for all layers. Admissable values
#' currently are "sumofsquares", "euclidean", "manhattan", and
#' "tanimoto". Default is to use "sumofsquares" for continuous data,
#' and "tanimoto" for factors.
#' @param mode type of learning algorithm.
#' @param cores number of cores to use in the "pbatch" learning mode. The
#' default, -1, corresponds to using all available cores.
#' @param init list of matrices, initial values for the
#' codebook vectors. The list should have the same length as the data
#' list, and corresponding numbers of variables (columns). Each list
#' element should have a number of rows corresponding to the number of
#' units in the map.
#' @param normalizeDataLayers boolean, indicating whether
#' \code{distance.weights} should be calculated (see details section).
#' If \code{normalizeDataLayers == FALSE} the user weights
#' are applied to the data immediately.
#'
#' @return An object of class "kohonen" with components
#' @param    data data matrix, only returned if \code{keep.data == TRUE}.
#' @param    unit.classif winning units for all data objects,
#'   only returned if \code{keep.data == TRUE}.
#' @param    distances distances of objects to their corresponding winning
#'   unit, only returned if \code{keep.data == TRUE}.
#' @param    grid the grid, an object of class \code{somgrid}.
#' @param    codes a list of matrices containing codebook vectors.
#' @param    changes matrix of mean average deviations from code vectors;
#' every map corresponds with one column.
#' @param {alpha, radius, user.weights, whatmap, maxNA.fraction}
#'
#'
#'    input arguments presented to the function.
#' @param    distance.weights if \code{normalizeDataLayers} weights to
#'  equalize the influence of the individual data layers, else a vector
#'   of ones.
#' @param    dist.fcts distance functions corresponding to all layers of the
#'  data, not just the ones indicated by the \code{whatmap} argument.
#' @export
#'
#' @examples
#'
#' #TODO an example for trajectories should be explained here
#'
#'
#' @references R. Wehrens and L.M.C. Buydens, J. Stat. Softw. 21 (5), 2007;
#' R. Wehrens and J. Kruisselbrink, submitted, 2017.
#'
#' @seealso   \code{\link[kohonen]{supersom}}
#'
#'
som <- function(data,
                     grid = somgrid(),
                     rlen = 100,
                     alpha = c(0.05, 0.01),
                     radius = quantile(nhbrdist, 2/3),
                     whatmap = NULL,
                     user.weights = 1,
                     maxNA.fraction = 0L,
                     keep.data = TRUE,
                     dist.fcts = NULL,
                     mode = c("online", "batch", "pbatch"),
                     cores = -1,
                     init,
                     normalizeDataLayers = TRUE
){

  # grid <- kohonen::check.somgrid(grid)
  nhbrdist <- unit.distances(grid)

  som_result <- kohonen::supersom(data,
                       grid = grid,
                       rlen = rlen,
                       alpha = alpha,
                       radius = radius,
                       whatmap = whatmap,
                       user.weights = user.weights,
                       maxNA.fraction = maxNA.fraction,
                       keep.data = keep.data,
                       dist.fcts = dist.fcts,
                       mode = mode,
                       cores = cores,
                       init,
                       normalizeDataLayers = normalizeDataLayers)

  return(som_result)
}


#' wrapping the \code{\link[kohonen]{somgrid}}function to avoid any errors
#'
#'
#' @param xdim dimensions of the grid, horizontal axis.
#' @param ydim dimensions of the grid, vertical axis.
#' @param topo choose between a hexagonal or rectangular topology.
#' @param neighbourhood.fct choose between bubble and gaussian neighbourhoods when training a SOM.
#' @param toroidal logical, whether the grid is toroidal or not.
#' If not provided to the unit.distances function,
#' the information in the grid object will be used.
#'
#'
#'
#'
#' @return
#' @param grid an object of class \code{somgrid}.
#' @export
#'
somgrid <- function(xdim = 8, ydim = 6, topo = c("rectangular", "hexagonal"),
                    neighbourhood.fct = c("bubble", "gaussian"), toroidal = FALSE) {

  grid <- kohonen::somgrid(xdim = xdim,
                   ydim = ydim,
                   topo = topo,
                   neighbourhood.fct = c("bubble", "gaussian"),
                   toroidal = FALSE)
  return(grid)
}


#' a wrapper over \link[kohenen]{unit.distances}
#'
#'
#' @param grid an object of class \code{somgrid}.
#' @param torodial logical, whether the grid is toroidal or not.
#' If not provided to the unit.distances function,
#' the information in the grid object will be used.
#'
#' @return
#' Function unit.distances returns a (symmetrical) matrix containing distances.
#' When grid$n.hood equals "circular", Euclidean distances are used; for
#' grid$n.hood is "square" maximum distances. For toroidal maps
#' (joined at the edges) distances are calculated for the shortest path.
#'
#' @export
#'
#'
#'
unit.distances <- function(grid, torodial){
  mat <- kohonen::unit.distances(grid = grid, toroidal = torodial)

  return(mat)


}
