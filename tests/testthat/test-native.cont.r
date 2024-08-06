test_that("struct2pdb works correctly from struct file", {
  # Load gro file
  gro_file <- system.file("extdata", "HIF2a.gro", package = "SOMMD")
  struct <- read.struct(gro_file)

  # Execute Function
  ca.inds <- which(struct$atom$elety=="CA")
  sele.dists <- native.cont(struct=struct, distance=0.4, atoms=ca.inds)

  expected_dists <- c(2, 114, 226, 338, 450, 562, 674, 786, 898, 1010, 1122, 1234, 1346,
                      1458, 1570, 1682, 1794, 1906, 2018, 2130, 2242, 2354, 2466, 2578,
                      2690, 2802, 2914, 3026, 3138, 3250, 3362, 3474, 3586, 3698, 3810,
                      3922, 4034, 4146, 4258, 4370, 4482, 4594, 4706, 4818, 4930, 5042,
                      5154, 5266, 5378, 5490, 5602, 5714, 5826, 5938, 6050, 6162, 6274,
                      6386, 6498, 6610, 6722, 6834, 6946, 7058, 7170, 7282, 7394, 7506,
                      7618, 7730, 7842, 7954, 7959, 8066, 8178, 8290, 8402, 8514, 8547,
                      8626, 8738, 8850, 8962, 9074, 9186, 9298, 9410, 9522, 9634, 9746,
                      9858, 9970, 10082, 10194, 10306, 10418, 10530, 10642, 10754, 10866,
                      10978, 11090, 11202, 11314, 11426, 11538, 11650, 11762, 11874, 11986,
                      12098, 12210)
  expect_equal(sele.dists, expected_dists, tolerance = 1e-6)

})


test_that("struct2pdb works correctly from trj file", {
  gro_file <- system.file("extdata", "HIF2a.gro", package = "SOMMD")
  xtc_file <- system.file("extdata", "HIF2a-MD.xtc", package = "SOMMD")
  trj <- read.trj(xtc_file, gro_file)
  struct <- read.struct(gro_file)
  #Select protein and ligand atoms
  protein.sele <- which(trj$top$resid!="020")
  ligand.sele <- which(trj$top$resid=="020")
  # Execute Function
  ca.inds <- which(trj$top$elety=="CA")
  sele.dists <- native.cont(struct=struct, distance=0.4, atoms=ca.inds)

  expected_dists <- c(2, 114, 226, 338, 450, 562, 674, 786, 898, 1010, 1122, 1234, 1346,
                      1458, 1570, 1682, 1794, 1906, 2018, 2130, 2242, 2354, 2466, 2578,
                      2690, 2802, 2914, 3026, 3138, 3250, 3362, 3474, 3586, 3698, 3810,
                      3922, 4034, 4146, 4258, 4370, 4482, 4594, 4706, 4818, 4930, 5042,
                      5154, 5266, 5378, 5490, 5602, 5714, 5826, 5938, 6050, 6162, 6274,
                      6386, 6498, 6610, 6722, 6834, 6946, 7058, 7170, 7282, 7394, 7506,
                      7618, 7730, 7842, 7954, 7959, 8066, 8178, 8290, 8402, 8514, 8547,
                      8626, 8738, 8850, 8962, 9074, 9186, 9298, 9410, 9522, 9634, 9746,
                      9858, 9970, 10082, 10194, 10306, 10418, 10530, 10642, 10754, 10866,
                      10978, 11090, 11202, 11314, 11426, 11538, 11650, 11762, 11874, 11986,
                      12098, 12210)
  expect_equal(sele.dists, expected_dists, tolerance = 1e-6)

})

test_that("struct2pdb works correctly computing intermolecular distances", {
  # Load gro file
  gro_file <- system.file("extdata", "HIF2a.gro", package = "SOMMD")
  struct <- read.struct(gro_file)
  #Select protein and ligand atoms
  protein.sele <- which(struct$atom$resid!="020")
  ligand.sele <- which(struct$atom$resid=="020")
  #Select heavy atoms
  heavy.atoms <- which(startsWith(struct$atom$elety, "H")==FALSE)
  #Choose only native contacts
  sele.dists <- native.cont(struct=struct, distance=0.4, mol.2=ligand.sele, atoms=heavy.atoms)

  expected_dists <-  c(444, 1263, 2167, 2231, 2482, 2484, 3071, 3135, 3270, 3387, 3388, 3530,
                       3727, 3975, 4434, 4879, 5338, 5783, 6242, 6389, 6409, 6886, 7160, 7589,
                       8266, 8267, 8452, 8453, 8455, 8479, 8714, 8715, 9396, 9397, 9598, 9599,
                       9618, 10263, 10300, 10301, 10522, 10523, 10928, 10929, 10930, 11666, 11830,
                       11833, 11834, 11862, 11863, 12111, 12570, 12736, 12737, 12785, 12786, 12787,
                       12973, 13638,13639, 13640, 13641, 13642, 13669, 13670, 13671, 13674, 13877,
                       13919, 14541, 14542, 14546, 14574, 14575, 15266, 15282, 15812, 15889, 15902,
                       16631, 16691, 16692, 16694, 16696, 16697, 16716, 16718, 16720, 17620, 17622,
                       17624, 17625, 17626, 17656, 17697)
  expect_equal(sele.dists, expected_dists, tolerance = 1e-6)

})
