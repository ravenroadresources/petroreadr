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
  expect_equal(extract_wellname("WELL.  WELL .Abc-01       : WELL"), "WELL .Abc-01")
  expect_equal(extract_wellname("WELL . ANY ET AL 12-34-12-34 :WELL"), "ANY ET AL 12-34-12-34")
  expect_equal(extract_wellname("WELL. ANY ET AL 12-34-12-34 :WELL"), "ANY ET AL 12-34-12-34")
  expect_equal(extract_wellname("WELL . ANY ET AL 12-34-12-34 :WELL"), "ANY ET AL 12-34-12-34")
  expect_equal(extract_wellname("WELL . ANY ET 12-34-12-34 :WELL"), "ANY ET 12-34-12-34")
  expect_equal(extract_wellname("WELL. A 10-16-39-3 :Well"), "A 10-16-39-3")
})


# read_las
test_that("testing read_las with examples las files: log names", {
  expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example1.las", package = "petroreadr")
    temp <- read_las(lasfile)
    colnames(temp)
  }, c("WELL", "DEPT", "DT", "RHOB", "NPHI", "SFLU", "SFLA", "ILM", "ILD"))
  expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example2.las", package = "petroreadr")
    temp <- read_las(lasfile)
    colnames(temp)
  }, c("WELL", "DEPT", "RHOB", "NPHI", "MSFL", "SFLA", "ILM", "ILD", "SP"))
  expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example3.las", package = "petroreadr")
    temp <- read_las(lasfile)
    length(colnames(temp))
  }, 36 + 1) # 36 log tracks + WELL
  expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example4.las", package = "petroreadr")
    temp <- read_las(lasfile)
    colnames(temp)
  }, c("WELL", "TIME", "BFR1", "BSG1"))
  expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example5.las", package = "petroreadr")
    temp <- read_las(lasfile)
    colnames(temp)
  }, c("WELL", "INDEX.na", "TIME.SEC", "DATE_1.", "DATE_2.", "TIME_1.", "TIME_2.", "TIME_3.", "TPRE.S", "QDPF.KPAA", "BIT.MM"))
})
