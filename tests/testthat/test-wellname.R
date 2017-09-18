context("testing wellname from .las files")

#-------------------------------------------------------------------------------

# extract_wellname
test_that("Test extract_wellname with different strings", {
  expect_equal(extract_wellname("WELL.  Abc-01       : WELL"), "Abc-01")
  expect_equal(extract_wellname("WELL .  Abc-01       : WELL"), "Abc-01")
  expect_equal(extract_wellname("WELL.  Abc 01       : WELL"), "Abc 01")
  expect_equal(extract_wellname("WELL.  Abc 01 ST 03b       : WELL"), "Abc 01 ST 03b")
  expect_equal(extract_wellname("WELL .  Well-01       : WELL"), "Well-01")
  expect_equal(extract_wellname("WELL.  WELL. 01       : WELL"), "WELL. 01")
  #expect_equal(extract_wellname("WELL.  WELL .Abc-01       : WELL"), "WELL .Abc-01")
})
