#' Plot clusterd SOM map
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#' @param som_obj  should be a `kohonen` object
#' @param cluster can be a vector showing the number of cluster of each neurons or \code{NULL}
#' @param cluster_method if \code{cluster} is \code{NULL} , either `"hier"` as hierarchical methods or
#'  `"kmeans"` can be chosen to do a basic clustering based on `clustering_parameter`
#' @param clustering_parameter is a list of to basic clustering hyper-parameter, first number of clusters
#'  and second method of clustering in `hier` or algorithm in `kmeans methods`
#' @param bgcols hues distinguish clusters can be introduced here
#' @param shape shape of neurons , `straight` plots neurons as hexagons  and `round` as cricles
#'
#' @return
#' @export
#'
#' @examples
#'
#'
"plot_clustered_map" <- function(som_obj ,
                                 cluster = NULL ,
                                 cluster_method = c("hier", "kmeans"),
                                 clustering_parameter = c( 5 , "ward.D"  ),
                                 bgcols = NULL,
                                 shape = c("straight" , "round")
) {
  ## check the som_obj is a `kohonen` object
  if (class(som_obj) != "kohonen") {
    stop( red$bold("The input should be a " %+% yellow("Kohonon object")))
  }

   ## check whether cluster number are introduced or the method of clustering has been chosen
  ## and ack accordingly
  if (is.null(cluster) && (length(cluster_method) != 1) ) {
    cat(yellow$bold("NOTE: ") %+% "since neither cluster numbers introduced,
    nor the clustering method choosed \n
        hierarchical method will use in clustering")
    cluster_method <- "hier"

  } else if (is.null(cluster) && (length(cluster_method) == 1)){
    chosen_method <-match.arg(cluster_method)
    if ((chosen_method == "hier") &&
        !(clustering_parameter[2] %in% c("ward.D", "ward.D2",
                             "single", "complete",
                             "average" , "mcquitty" ,
                             "median" , "centroid" ))){
      cat(yellow$bold("Warning: ") %+% yellow("the clustering parameter chosen ") %+%
      clustering_parameter  %+% yellow(" is not compatible with
      hierarchical method, and it  is replaced by \"ward.D\" to continue the process,
      alternatively one of
      \"ward.D\", \"ward.D2\",
      \"single\", \"complete\",
      \"average\" , \"mcquitty\" ,
      \"median\" , \"centroid\"  can be picked"))
      clustering_parameter[2] <- "ward.D"
    } else if ((chosen_method == "kmeans") &&
               !(clustering_parameter[2] %in% c("Hartigan-Wong",
                                                "Lloyd",
                                                "Forgy",
                                                "MacQueen"))){
      cat(yellow$bold("Warning: ") %+% yellow("the clustering parameter chosen ") %+%
            clustering_parameter  %+% yellow(" is not compatible with
      kmeans method, and it  is replaced by \"Hartigan-Wong\" to continue the process,
      alternatively one of
      \"Hartigan-Wong\",\"Lloyd\",\"Forgy\",\"MacQueen\" "))
          clustering_parameter[2] <- "Hartigan-Wong"
    } else if (!(chosen_method %in% c("hier" , "kmeans"))) {
      stop(yellow("the chosen method ") %+%
          cluster_method  %+% yellow(" is not defined in this function,
          please  do clustering seperately and put as cluster aurguments"))
    }
    if(cluster_method == "hier"){
      cluster <- as.vector(cutree(hclust(dist(som_obj$codes[[1]]),
                               method = clustering_parameter[2]),
                        clustering_parameter[1]))
    } else {
      cluster <-kmeans(som_obj$codes[[1]],
                       centers = clustering_parameter[1],
                       algorithm = clustering_parameter[2])
      cluster <-  cluster$cluster
    }

  }


    if (is.null(bgcols)) {
      bgcols <-   c("#bf45e9", "#92ec67" ,
                    "#4e77e9","#a65e1c" ,
                    "#459f64", "#c73038",
                    "#908fc9", "#ec66a6")}

  plot(som_obj, type = "mapping",
       bgcol = bgcols[as.vector(cluster)],
       col = rgb(0,0,0,0),
       shape =shape)
  add.cluster.boundaries(som_obj, cluster, lwd = 6)
}


