#' add populaiton of each neuron to the som plots
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#' @param som_obj  should be a `kohonen` object
#' @param text  logical , if TRUE print the population of each neuron as number
#' @param shape  a chosen shape size of which show the popluation of each neurons
#'
#' @return
#'
#' @export
#'
#' @examples
#' ###
#' library(kohonen)
#' data(wines)
#' som_wines <- som(scale(wines), grid = somgrid( 6 , 6, "hexagonal))
#' plot_clustered_map (som_wines,
#' cluster_method "hier",
#' clustering_parameter = c( 5 , "ward.D"  ),
#' shape = "straight")
#' add_neuron_population(som_wines , shape = "round" )

"add_neuron_population" <- function(som_obj , text = FALSE , shape = c("hex" , "round")) {

  neuron_population <- as.vector(table(som_obj$unit.classif))
  scaled_neuron_populaton <- minmax(neuron_population)
  shifted_neuron_population <-  1.3 + 2 * scaled_neuron_populaton

  ## print a number of population of each neuron instead of shapes
  if ( text ) {
  for ( i  in 1:dim(som_obj$grid$pts)[1]) {
    text( som_obj$grid$pts[ i , 1 ] , som_obj$grid$pts[ i , 2 ] , labels = neuron_population[ i ])
  }

  } else {
    if ( shape == "round") {
      shapes <-  16
    } else if (shape == "hex") {
      shapes = "⬢"
    } else if (( typeof(shape) != "charcter") && (length(shape) != 1)) {
      stop("an inappropiate shape has been chosen")
    } else { shapes = shape}

    for ( i  in 1:dim(som_obj$grid$pts)[1]) {
      points( som_obj$grid$pts[ i , 1 ] ,
              som_obj$grid$pts[ i , 2 ] ,
              col = "black" , pch = shapes ,
              cex = shifted_neuron_population [i] + .3)
      points( som_obj$grid$pts[ i , 1 ] ,
              som_obj$grid$pts[ i , 2 ] ,
              col = "white" , pch = shapes ,
              cex = shifted_neuron_population [i] + .15)
      points( som_obj$grid$pts[ i , 1 ] ,
              som_obj$grid$pts[ i , 2 ] ,
              col = "black",
              pch = shapes ,
              cex = shifted_neuron_population [i])
    }



  }
}



#' Title
#'
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#' @param som_obj  should be a `kohonen` object
#' @param path the path of neuron want to show on the plot
#' @param line_thickness the thickness of connecting lines same as \code{lwd} argument in \code{lines}
#' @param point_size the size of points same as  arguments \code{cex} argument in \code{points}
#'
#' @return
#' @export
#'
#' @examples
#' library(kohonen)
#' data(wines)
#' som_wines <- som(scale(wines), grid = somgrid( 6 , 6, "hexagonal))
#' plot_clustered_map (som_wines,
#'  cluster_method "hier",
#'  clustering_parameter = c( 5 , "ward.D"  ),
#'  shape = "straight" )
#' add_evolution_trace(som_wines , path)
"add_evolution_trace" <- function(som_obj , path , line_thickness = 3 , point_size = 2) {


  X <- NULL
  Y <- NULL
  length_path <- length(path)
  for (i in 1:(length(path) - 1)){

    tempX <- c(som_obj$grid$pts[ path[i] , 1 ] , som_obj$grid$pts[ path[ i + 1 ] , 1 ])
    tempY <- c(som_obj$grid$pts[ path[i] , 2 ] , som_obj$grid$pts[ path[ i + 1 ] , 2 ])
    lines(tempX , tempY, lwd = line_thickness)
    X <- c( X , som_obj$grid$pts[ path[i] , 1 ])
    Y <- c( Y , som_obj$grid$pts[ path[i] , 2 ])
  }
  X <- c( X , som_obj$grid$pts[ path[ i + 1 ] , 1 ] )
  Y <- c( Y , som_obj$grid$pts[ path[ i + 1 ] , 2 ] )
  points(X,Y, pch=16, cex=point_size)
  points(X,Y, pch=16, col=COL(length(X)), cex = point_size - 0.7)



}
