test_that("Pre Align the Trajectory", {
  Test_Pre_Align <- c(xyz, resno, b, chain, id)
  expect_equal(xyz, resno, b, chain, id, Test_Pre_Align)
})
