# Funzione di test per cluster.pathways
test_that("cluster.pathways works correctly in time independent mode", {
  # Caricare il SOM
  som_file <- system.file("extdata", "SOM.rds", package = "SOMMD")
  som_model <- readRDS(som_file)
  # Definire le traiettorie
  start <- c(1, 51, 101)
  end <- c(50, 100, 150)
  
  # Eseguire la funzione
  result <- cluster.pathways(SOM = som_model, start = start, end = end, time.dep="independent")
  
  #Verify the obtained merge
  expected_merge <- matrix(c(-2, -1, -3, 1), nrow=2)
  expect_equal(result$merge, expected_merge, tolerance = 1e-6)

  #Verify the obtained height
  expected_height <- c(0.8317691, 3.0729398)
  expect_equal(result$height, expected_height, tolerance = 1e-6)

})
