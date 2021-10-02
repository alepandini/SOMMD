test_that("plot som map which hue according to a given property", {

  load("data/pdb_and_tarj_object.RData")

  som_trj <- SOMMD::som(trj1 , grid =  somgrid( 6 , 6, "hexagonal"))
  dummy_property_nuerons <-  rnorm(36 , mean = 8 , sd  = 4)

  #  property vector for each neurons
  plot_property(som_trj , property = dummy_property_nuerons )

  plot_property(som_trj , property = dummy_property_nuerons ,shape = "straight",
                palette_name =  colorRampPalette(c("blue", "white", "red")))


  ##  property vector for each instance
   dummy_property_observation <-  rnorm(101 , mean = 8 , sd  = 4)
    plot_property(som_trj , property = dummy_property_observation, shape = "straight",
     palette_name =  colorRampPalette(c("blue", "white", "red")))


    plot_property(som_trj ,
                  property = dummy_property_observation,
                  shape = "straight",
                  FUN = median,
     palette_name =  colorRampPalette(c("blue", "white", "red")))
})
