#' Plot a heatmap based on a given property
#'
#'
#' @author Hamid Davoukhani \email{h.davoudkhani@@gmail.com}
#'
#' @param som_obj a \code{kohonen} object
#' @param property a vector of property for either each neurons or each observation
#' @param FUN  when length of property vector is equal to number of observation,
#' this given argument use as a function to aggregate values for each neurons
#' @param main the title of the plot
#' @param heatkey logical, when is true, plot the spectrum of the property
#' @param palette_name palette color used to hue neurons
#' @param ...  other graphical arguments can be passed to plot funtion
#'
#' @return
#' @export
#'
#' @seealso \link{plot_clustered_map} ,  \link{\code{add_evolution_trace}} , \link{\code{add_neuron_population}}
#'
#' @examples

"plot_property" <- function(som_obj ,
                            property ,
                            FUN = mean ,
                            main = NULL,
                            heatkey = TRUE,
                            palette_name = NULL,
                            ...
) {

  ## preliminary checks of arguments
  if ( class(som_obj) != "kohonen") {
    stop("no kohonen object was given")
  }

  if( !(typeof(property) %in% c("double" , "integer")) ) {
    stop("property is not a vector or list of numerical values")
  } else if ( (length(property) != length(som_obj$unit.classif)) &&
             (length(property) != dim(som_obj$codes[[1]])[1])  ) {
    stop("the length of property vector is not as same number of neurons nor observation")
  }

  number_of_neurons <- dim(som_obj$codes[[1]])[1]
  property_vector <- as.vector(rep(0, number_of_neurons), mode = "double")

  ## calculate the value of each neurons when the property vector is given per observation
  if ( (length(property) == length(som_obj$unit.classif)) ) {
    ## check the fun given is R function or not
    f <- match.fun(FUN)
    for(i in seq(1:number_of_neurons)) {
      property_vector[i] <- f(property[som_obj$unit.classif == i])
      ## check the output should check to be numerical
      if(typeof(property_vector) != "double") {
        stop(gettextf("'%s doesn't generate numerical value" , deparse(FUN)), domain = NA)
      }

    }
  } else {
    property_vector <- property
  }


  ## plot the property heatmap
  plot(som_obj ,
       type = "property",
       property = property_vector ,
       main = main ,
       heatkey = heatkey,
       palette.name = palette_name ,
       ...
       )

}
