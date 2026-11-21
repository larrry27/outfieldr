test_that("batted ball data pulls as expected", {
  
  meidrothBatted <- getBattedBallData(805367, 2026, 200, 11)
  
  expect_equal(ncol(meidrothBatted),14)
  expect_equal(nrow(meidrothBatted),191)
  
})

test_that("error handling for arguments is correct", {
  
  expect_error(getBattedBallData("chasemeidroth", 2026, 200, 11),"playerid must be a positive integer\\.")
  expect_error(getBattedBallData(-4, 2019, 200, 11),"playerid must be a positive integer\\.")
  expect_error(getBattedBallData(805367, "2026al2bsilverslugger", 200, 11),"Year value must be an integer greater than or equal to 2020\\.")
  expect_error(getBattedBallData(805367, 2019, 200, 11),"Year value must be an integer greater than or equal to 2020\\.")
  expect_error(getBattedBallData(805367, 2026, "twohundred", 11),"n must be a positive integer\\.")
  expect_error(getBattedBallData(805367, 2026, -5, 11),"n must be a positive integer\\.")
  expect_error(getBattedBallData(805367, 2026, 200, "CWS"),"Invalid parkid\\. Must be a positive integer between 1 and 37\\.")
  expect_error(getBattedBallData(805367, 2026, 200, 40),"Invalid parkid\\. Must be a positive integer between 1 and 37\\.")
  expect_error(getBattedBallData(805367, 2026, 200, -4),"Invalid parkid\\. Must be a positive integer between 1 and 37\\.")
  
})

test_that("if no results are pulled from statcast search, don't return anything", {
  
  expect_error(getBattedBallData(805367, 2024, 200, 11),"No data was found for specified player, year, and park\\.")
  
})