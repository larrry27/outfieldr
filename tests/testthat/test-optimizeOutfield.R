test_that("error handling for arguments is correct", {
  
  expect_error(optimizeOutfield("pca",803011,671976,695731,250,2019,11),"batterId must be a positive integer\\.")
  expect_error(optimizeOutfield(-4,803011,671976,695731,250,2019,11),"batterId must be a positive integer\\.")
  expect_error(optimizeOutfield(691718,"sammy",671976,695731,250,2019,11),"\\(LF\\) playerid must be a positive integer, or within \\(-7,-8,-9\\)\\.")
  expect_error(optimizeOutfield(691718,-4,671976,695731,250,2019,11),"\\(LF\\) playerid must be a positive integer, or within \\(-7,-8,-9\\)\\.")
  expect_error(optimizeOutfield(691718,803011,"tristan",695731,250,2019,11),"\\(CF\\) playerid must be a positive integer, or within \\(-7,-8,-9\\)\\.")
  expect_error(optimizeOutfield(691718,803011,-4,695731,250,2019,11),"\\(CF\\) playerid must be a positive integer, or within \\(-7,-8,-9\\)\\.")
  expect_error(optimizeOutfield(691718,803011,671976,"monty",250,2019,11),"\\(RF\\) playerid must be a positive integer, or within \\(-7,-8,-9\\)\\.")
  expect_error(optimizeOutfield(691718,803011,671976,-4,250,2019,11),"\\(RF\\) playerid must be a positive integer, or within \\(-7,-8,-9\\)\\.")
  expect_error(optimizeOutfield(691718,803011,671976,695731,250,"2sdf26",11),"Year value must be an integer greater than or equal to 2020\\.")
  expect_error(optimizeOutfield(691718,803011,671976,695731,250,2019,11),"Year value must be an integer greater than or equal to 2020\\.")
  expect_error(optimizeOutfield(691718,803011,671976,695731,"twofifty",2026,11),"n must be a positive integer\\.")
  expect_error(optimizeOutfield(691718,803011,671976,695731,-250,2026,11),"n must be a positive integer\\.")
  expect_error(optimizeOutfield(691718,803011,671976,695731,250,2026,"CWS"),"Invalid parkid\\. Must be a positive integer between 1 and 37\\.")
  expect_error(optimizeOutfield(691718,803011,671976,695731,250,2026,40),"Invalid parkid\\. Must be a positive integer between 1 and 37\\.")
  expect_error(optimizeOutfield(691718,803011,671976,695731,250,2026,-11),"Invalid parkid\\. Must be a positive integer between 1 and 37\\.")
  
})