test_that("fielder stats pull as expected", {
  
  petersStats <- getFielderStats(671976, 2026)
  
  expect_equal(ncol(petersStats),9)
  expect_equal(nrow(petersStats),1)
})

test_that("error handling for arguments is correct", {
  
  expect_error(getFielderStats(-4, 2026),"playerid must be a positive integer, or within \\(-7,-8,-9\\)\\.")
  expect_error(getFielderStats("tristanpeters", 2026),"playerid must be a positive integer, or within \\(-7,-8,-9\\)\\.")
  expect_error(getFielderStats(671976, "2df026"),"Year value must be an integer greater than or equal to 2020\\.")
  
})

test_that("if no results are pulled from statcast search, don't return anything", {
  
  expect_error(getFielderStats(671976, 2020),"No data was found for specified player and year\\.")
               
})