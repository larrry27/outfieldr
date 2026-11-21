test_that("competitive area is pulled as expected", {
  
  testArea <- getCompetitiveArea(0,250,30)
  
  expect_equal(ncol(testArea),2)
  expect_equal(nrow(testArea),45225)
  
})