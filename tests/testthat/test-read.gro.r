test_that("read.gro correctly parses .gro file", {
  # Specifica il percorso del file di esempio
  gro_file <- system.file("extdata/", "HIF2a.gro", package = "SOMMD")
  
  # Leggi il file .gro
  gro <- read.gro(gro_file)
  
  # Verifica che l'output sia di classe 'gro'
  expect_s3_class(gro, "gro")
  
  # Verifica che i componenti dell'output siano corretti
  expect_true(is.data.frame(gro$atom))
  expect_true(is.matrix(gro$xyz))
  expect_true(is.numeric(gro$box))
  
  # Verifica che le colonne del dataframe siano corrette
  expected_colnames <- c("resno", "resid", "elety", "eleno", "x", "y", "z", "Vx", "Vy", "Vz")
  expect_equal(colnames(gro$atom), expected_colnames)
  
  # Verifica che i dati siano correttamente formattati
  expect_true(all(sapply(gro$atom[, c("resno", "eleno", "x", "y", "z", "Vx", "Vy", "Vz")], is.numeric)))
  expect_true(all(sapply(gro$atom[, c("resid", "elety")], is.character)))

  #Verify the coordinates of an atom in trajectory
  result <- as.character(gro$atom[100,c(1:7)])
  expected_result <- c(244, "PHE", "O", 100, 5.287, 7.315, 3.876)
  expect_equal(result, expected_result, tolerance = 1e-6)

})

