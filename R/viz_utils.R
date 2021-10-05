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
#' load("data/pdb_and_tarj_object.RData")
#'
#' ## neuron population
#'
#' som_trj <- som(trj1 , grid =  somgrid( 6 , 6, "hexagonal"))
#' som_hc <- cutree(hclust(dist(som_trj$codes[[1]] ), method = "ward.D") , 5)
#' plot_clustered_map (som_trj,
#'                     cluster = som_hc,
#'                     shape = "straight" )
#' add_neuron_population(som_trj , shape = "round" )
#'
"add_neuron_population" <- function(som_obj , text = FALSE , shape = c("hex" , "round")) {

  if ( class(som_obj) != "kohonen") {
    stop("no kohonen object was given")
  }
  # extract the variables needs in next step and scale them to have more even size
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
      ## TODO daes work
    } else if (shape == "hex") {
      shapes = "⬢"
    } else if (( typeof(shape) != "charcter") && (length(shape) != 1)) {
      stop("an inappropiate shape has been chosen")
    } else { shapes = shape}
## iterate  for each neuron and print 3 co-centered circle for aesthetic aspect.
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



#' add_evolution_trace
#'
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#' @param som_obj  should be a `kohonen` object
#' @param path the path of neuron want to show on the plot
#' @param line_thickness the thickness of connecting lines same as \code{lwd} argument in \code{lines}
#' @param point_size the size of points same as  arguments \code{cex} argument in \code{points}
#' @param point_col color of center of points
#'
#' @return
#' @export
#'
#' @examples
#' path <- c( 16 , 4 , 6, 29, 36 , 1)
#' som_trj <- som(trj1 , grid =  somgrid( 6 , 6, "hexagonal"))
#' som_hc <- cutree(hclust(dist(som_trj$codes[[1]] ), method = "ward.D") , 5)
#' plot_clustered_map (som_trj,
#' cluster = som_hc,
#' shape = "straight" )
#' add_evolution_trace(som_trj , path)
"add_evolution_trace" <- function(som_obj , path , point_col = "red" ,  line_thickness = 3 , point_size = 2) {

  if ( class(som_obj) != "kohonen") {
    stop("no kohonen object was given")
  }
  ## define empty variables
  X <- NULL
  Y <- NULL
  length_path <- length(path)
  ## iterate for number of point minus one
  for (i in 1:(length(path) - 1)){
    ## temperory save coordinate of neurons to draw a line between them.
    tempX <- c(som_obj$grid$pts[ path[i] , 1 ] , som_obj$grid$pts[ path[ i + 1 ] , 1 ])
    tempY <- c(som_obj$grid$pts[ path[i] , 2 ] , som_obj$grid$pts[ path[ i + 1 ] , 2 ])
    lines(tempX , tempY, lwd = line_thickness)
    ## concatenate coordinates for draw points in next step
    X <- c( X , som_obj$grid$pts[ path[i] , 1 ])
    Y <- c( Y , som_obj$grid$pts[ path[i] , 2 ])
  }
  ## add last point coordinates
  X <- c( X , som_obj$grid$pts[ path[ i + 1 ] , 1 ] )
  Y <- c( Y , som_obj$grid$pts[ path[ i + 1 ] , 2 ] )
  ## print two  co-centered and filled circle for each stored coordinate
  points(X,Y, pch=16, cex=point_size)
  points(X,Y, pch=16, col= point_col, cex = point_size - 0.7)



}


#' Highlight a cluster and its attributes
#'
#' @description
#' Highlight a clusters and add note at center of cluster and a property value in a different center of clusters
#'
#' @param som_obj should be a `kohonen` object
#' @param clusters a vector of cluster neuron
#' @param cluster_number the cluster to be highlighted
#' @param label label print at the center of cluster
#' @param property_value a different value print at center of different neuron of cluster
#' @param property_color color property note, if `NULL` use `label_color`
#' @param label_color color of label note, default is `black`
#' @param lwd thickness of both notes
#' @param col the color draw a boundary around the cluster
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'  load("data/pdb_and_tarj_object.RData")
#' set.seed(100)
#' som_trj <- SOMMD::som(trj1 , grid =  somgrid( 6 , 6, "hexagonal"))
#' dummy_property_nuerons <-  rnorm(36 , mean = 8 , sd  = 4)
#' som_hc <- cutree(hclust(dist(som_trj$codes[[1]] ), method = "ward.D") , 5)
#'
#'
#' plot_property(som_trj , property = dummy_property_nuerons ,shape = "straight",
#'               palette_name =  colorRampPalette(c("blue", "white", "red")))
#' highlight_a_cluster(som_trj,
#'                     som_hc ,
#'                     cluster_number = 3,
#'                     label = "cluster 3" ,
#'                     property_value = "66",
#'                     property_color = NULL,
#'                     label_color = "darkgreen",
#'                     col = "green",
#'                     lwd = 2,
#'                     cex = 2)
"highlight_a_cluster" <-  function(som_obj ,
                                   clusters ,
                                   cluster_number ,
                                   label = NULL ,
                                   property_value = NULL,
                                   property_color = NULL,
                                   label_color = "black",
                                   col = "red",
                                   lwd = 4,
                                    ... ) {

  if ( class(som_obj) != "kohonen") {
    stop("no kohonen object was given")
  }

  if( !(typeof(clusters) %in% c("double" , "integer")) ) {
    stop("clusters is not a vector or list of numerical values")
  } else if (  (length(clusters) != dim(som_obj$codes[[1]])[1])  ) {
    stop("the length of clusters vector is not as same number of neurons")
  } else if (  !(cluster_number %in% unique(clusters))){
    stop(gettextf(" there is no cluster with number '%d' in clusters vector", cluster_number))
  }
  if (typeof(label) != "character" && !(is.null(label)) ) {
    stop("label should be a charcter string")
  }
  ## generate a binary clustering vector
  binary_clustering <- ifelse(clusters == cluster_number , TRUE , FALSE)


  add.cluster.boundaries(som_obj, binary_clustering, lwd = lwd , col = col)

  if(!(is.null(label)) || !(is.null(property_value))) {

    ## separate cluster coordinate in som map
    cluster_grid_pts <- som_obj$grid$pts[binary_clustering,]
    ### calculate center of  the cluster
    mean_cluster <-  apply(cluster_grid_pts, 2 , mean)

    ## calculate of neuron distances from  the center of cluster
    diff_from_centre  <- apply(cluster_grid_pts , 1 ,
                               (function(x) sqrt((x[1] - mean_cluster[1]) ^ 2 +
                                                   (x[2] - mean_cluster[2]) ^ 2)) )
    ## store of the nearest neuron to the center of cluster
    centre <- cluster_grid_pts [ diff_from_centre == min(diff_from_centre), ]
    ## check whether a label should be printed or not
    if( !(is.null(label)) ){
      ## print the label in middle of cluster
      text(centre[ 1 ], centre[ 2 ] , labels = label, col = label_color , ...)
    }
    ## check whether a property in the centre of one of neurons
    if( !(is.null(property_value)) ) {
      ## check color allocated, if not using the label colors
      if(is.null(property_color)) { property_color = label_color}
      ## find center one neurons
      neuron_center <- cluster_grid_pts [ diff_from_centre == max(diff_from_centre), ]
      ## check if more than on filtered use one of  them
      if(dim(neuron_center)[1] != 1 ) {
        neuron_center <- neuron_center[1,]
      }
      ## print the property value
      text(x = neuron_center[1] , y = neuron_center[2], labels = property_value, col = property_color , ...)
    }

  }

}




