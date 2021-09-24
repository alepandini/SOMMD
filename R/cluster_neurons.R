#'  Find optimal clusters.
#'
#' @description
#' evaluate the number of  different clusters with indexes such as silhouette,
#' it also plot for each clusters
#'
#' @author Hamid davoudkhani \email{h.davoudkhani@@gmail.com}
#' @param som_obj     should be a `kohonen` object, a `matrix`, or
#' `dist` object of \link[stats]{dist()}
#' @param min_cluster minimum number of clusters shoulde be evaluate
#' @param max_cluster upper bond of number of clusters.
#' @param kohonen_object if the `som-obj` was not as a `kohonen`
#'  object it can call here to plot clustered som map
#'
#' @return
#' a list consist of all clusters content such as neuron clusters number, silhouette score and its summary.
#' @export
#'
#' @examples
#' library(kohonen)
#' data("wines)
#' som_wines <- som(scale(wines), grid = somgrid(5, 5, "hexagonal"))
#' (clusters_result <- cluster_neuron(som_wines, 3 , 7)
cluster_neuron <- function(som_obj,
                           min_cluster = 6,
                           max_cluster = 15 ,
                           kohonen_object = NULL) {

  Color_palette <- c("#bf45e9", "#92ec67" ,"#4e77e9","#a65e1c" ,"#459f64", "#c73038", "#908fc9", "#ec66a6")
  maroon <- crayon::make_style("maroon")

  ## a logical defined to check whether the */kohonen/* object introduced as input
  ## which capable to plot clustered som map
  plot_clustered_som <-  TRUE

  ## null  object which later outputs of different part assigned to that
  output <- NULL

  # only clustered_som_map can be plotted if the Kohonen object assigned as a input
  if (is.null(kohonen_object) | class(kohonen_object) != "kohonen" | class(som_obj) != "kohonen"){
    plot_clustered_som <- FALSE
  }

  ## preliminary check of inputs:
  if (!( typeof(max_cluster) == "integer" || typeof(max_cluster) == "double") &&
      ( typeof(min_cluster) == "double" || typeof(min_cluster) == "integer"  )) {
    stop(cat(maroon("The min or max number of cluster is not numerical, please correct that\n")))
  }
  if (min_cluster > max_cluster) {
    stop(cat(maroon("The upper bond number of cluster mistekenly was chosen less than lower bond, please correct that\n")))
  }

  if (min_cluster <  2) {
    stop(cat(maroon("The lower  bond number of cluster mistekenly was chosen less than 2, please correct that\n")))
  }



  ## base on type of input different preparation should be placed
  if(class(som_obj) == "kohonen"){

    # extract neurons from Kohonen object
    neurons_attrbutes <- som_obj$codes[[1]]

    #calculating distance matrix from neurons attributes
    dist_neurons <- dist(neurons_attrbutes )

    cat (yellow$bold("NOTE: ") %+% "the method used to generate distance matrix was euclidean, \n",
          "     however, if other method are considered, replace" %+% crayon::yellow$bold(" kohonen object ") %+%
          "with " %+% crayon::green("dist object ") %+% "or\n"
        %+% crayon::green$bold("                dist(<som object>$codes[[1]] , <chosen method>)\n"))

    # plot clustered som map in evaluations because the som_obj is as Kohonen class
    kohonen_object <- som_obj
    plot_clustered_som <- TRUE
  } else if (class(som_obj)[1] == "matrix" && class(som_obj)[2] == "array") {
   if ( dim(som_obj)[1] < max_cluster) {
     stop(maroon("The number of observation are less than upper bond of number of clusters : `max_cluster`\n"))
     }
   #calculating distance matrix from the given matrix
   dist_neurons <- dist(som_obj)
   cat (yellow$bold("NOTE: ") %+% "the method used to generate distance matrix was euclidean, \n",
        "     however, if other method are considered, replace" %+% crayon::yellow$bold("  the given matrix ") %+%
          "with " %+% crayon::green("dist object ") %+% "or\n"
        %+% crayon::green$bold("                dist(<the matrix> , <chosen method>)\n"))

 }  else if (class(som_obj) == "dist") {
    #calculating distance matrix from the given matrix
    dist_neurons <- som_obj
    cat (yellow$bold("NOTE: ") %+% "the distance matrix was introduced directly")
  }else {
  stop( red$bold("The input should be in either of following format: \n"
                 %+% yellow("Kohonon object, dist object, or 2d matrix \n"))
  )
  }
  ## if only a single quantity of clusters would be considered to evaluate
  ## min and max should provide equal to that
  if (min_cluster == max_cluster) {

    cluster_numbers <- cutree(hclust(dist_neurons, method = "average"), min_cluster)
    silhoutte_obj <- cluster::silhouette(cluster_numbers, dist_neurons)
    summary_silhoutte <- summary(silhoutte_obj)

    if (plot_clustered_som){
      opar <- par()
      par(mfrow = c(1,2))
      plot(kohonen_object, type = "mapping",
           bgcol = Color_palette[as.vector(cluster_numbers)],
           col = rgb(0,0,0,0),
           shape ='straight')
      add.cluster.boundaries(kohonen_object, cluster_numbers, lwd = 6)
    }
      plot(silhoutte_obj)
      par(mfrow = c(1,1 ))


  } else {
    ## number of different clustering should be checked
    number_of_test <- (max_cluster - min_cluster + 1)
    ## the result test will print at the end
    comparison_table <- tibble( "Number of clusters" = c(min_cluster:max_cluster),
                                "silhouette score \n average"= rep(0, number_of_test ) )
    # setting for generating multiple plots in one page
    opar <- par()
    if (number_of_test > 4  && plot_clustered_som ) {

      par(mfrow = c(2, 4))

    } else {
      number_of_plot_per_row <- floor(number_of_test/ 2 )
      par(mfrow = c( 2 , number_of_plot_per_row ))
    }
    ## calculate for each number of cluster
    for (i in min_cluster:max_cluster) {

      ## clustering
      cluster_numbers <- cutree(hclust(dist_neurons, method = "average"), i)
      ## cluster score with silhouette
      silhouette_obj <- cluster::silhouette(cluster_numbers, dist_neurons)
      ## storing summary of silhouette
      summary_silhouette  <- summary(silhouette_obj)

      ## storing in list of list
      temp_output <- list( cluster_number = cluster_numbers,
                           silhouette = silhouette_obj,
                           summary = summary_silhouette)

      output[[paste0(i)]] <- temp_output

      ## store mean of silhouette width in table
      comparison_table[comparison_table[, 'Number of clusters'] == i,
                       "silhouette score \n average" ] <- mean(silhouette_obj[,3])

      ## plot clustered som maps if kohonen object provided
      if(plot_clustered_som){

        plot(kohonen_object, type = "mapping",
             bgcol = Color_palette[as.vector(cluster_numbers)],
             col = rgb(0,0,0,0),
             shape ='straight')
        add.cluster.boundaries(kohonen_object, cluster_numbers, lwd = 6)

      }
      ## plot silhouette widths for each clusters
      plot(silhouette_obj)

    }
    print (comparison_table)
    ### reset the ploting settings
    par(mfrow = c(1,1))

   return(output)
  }



}
