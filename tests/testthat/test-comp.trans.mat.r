# Funzione di test per comp.trans.mat
test_that("comp.trans.mat works correctly", {
  # Load SOM
  som_file <- system.file("extdata", "SOM.rds", package = "SOMMD")
  som_model <- readRDS(som_file)

  
  # Execute Function
  tr_mat <- comp.trans.mat(som_model, start = 1)
  
  #Verify the obtained classification
  expected_values <- c(5, 4, 4, 3, 3, 3, 3, 3, 3, 3)
  expect_equal(sort(tr_mat, decreasing=TRUE)[1:10], expected_values, tolerance = 1e-6)

  #Verify the size of the data in the som object
  expected_trans <- c(  7,  13,  17,  22,  33,  38,  47,  48,  53,  58,  59,  63,  68,  72,  73,  90, 104, 105,
                      110, 115, 120, 125, 132, 136, 137, 146, 156, 158, 166, 167, 172, 177, 178, 183, 188, 193,
                      198, 213, 223, 230, 235, 240, 245, 251, 261, 266, 267, 271, 282, 283, 286, 291, 301, 302,
                      303, 307, 308, 312, 313, 323, 355, 360, 365, 370, 381, 382, 383, 386, 392, 397, 402, 406,
                      407, 412, 416, 417, 433, 448, 463, 480, 490, 495, 500, 501, 511, 522, 526, 528, 531, 532,
                      536, 541, 546, 547, 553, 559, 560, 563, 568, 569, 573, 605, 615, 625)    
  expect_equal(which(tr_mat>0), expected_trans, tolerance = 1e-6)

})

