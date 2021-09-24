test_that("clustering the som map with hierarchical clustering method", {
  library(kohonen)
  library(crayon)
  data("wines")
  som.wines <- som(scale(wines), grid = somgrid(5, 5, "hexagonal"))
  cluster_neuron(som.wines, min_cluster = 4, max_cluster = 5)
  expect_equal(2 * 2, 4)
})
