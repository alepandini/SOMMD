test_that("visualization utilities in viz_util", {
  load("data/pdb_and_tarj_object.RData")

  ## neuron population

  som_trj <- som(trj1 , grid =  somgrid( 6 , 6, "hexagonal"))
  som_hc <- cutree(hclust(dist(som_trj$codes[[1]] ), method = "ward.D") , 5)
  plot_clustered_map (som_trj,
                      cluster = som_hc,
                      shape = "straight" )
   add_neuron_population(som_trj , shape = "round" )




  ## add_evaluation_trace test
 path <- c( 16 , 4 , 6, 29, 36 , 1)
 som_trj <- som(trj1 , grid =  somgrid( 6 , 6, "hexagonal"))
 som_hc <- cutree(hclust(dist(som_trj$codes[[1]] ), method = "ward.D") , 5)
 plot_clustered_map (som_trj,
 cluster = som_hc,
 shape = "straight" )
 add_evolution_trace(som_trj , path)




  ## highlight cluster
  set.seed(100)
  som_trj <- SOMMD::som(trj1 , grid =  somgrid( 6 , 6, "hexagonal"))
  dummy_property_nuerons <-  rnorm(36 , mean = 8 , sd  = 4)
  som_hc <- cutree(hclust(dist(som_trj$codes[[1]] ), method = "ward.D") , 5)


  plot_property(som_trj , property = dummy_property_nuerons ,shape = "straight",
                palette_name =  colorRampPalette(c("blue", "white", "red")))
  highlight_a_cluster(som_trj,
                      som_hc ,
                      cluster_number = 3,
                      label = "cluster 3" ,
                      property_value = "66",
                      property_color = NULL,
                      label_color = "darkgreen",
                      col = "green",
                      lwd = 2,
                      cex = 2)





})
