context("testing wellname from .las files")

#-------------------------------------------------------------------------------

# extract_wellname
testthat::test_that("Test extract_wellname with different strings", {
  testthat::expect_equal(extract_lasinfo("WELL.  Abc-01       : WELL", "WELL"), "Abc-01")
  testthat::expect_equal(extract_lasinfo("WELL .  Abc-01       : WELL", "WELL"), "Abc-01")
  testthat::expect_equal(extract_lasinfo("WELL.  Abc 01       :WELL", "WELL"), "Abc 01")
  testthat::expect_equal(extract_lasinfo("WELL  .  Abc 01 ST 03b       : WELL", "WELL"), "Abc 01 ST 03b")
  testthat::expect_equal(extract_lasinfo("WELL .  Well-01       : WELL", "WELL"), "Well-01")
  testthat::expect_equal(extract_lasinfo("WELL.  WELL. 01       : WELL", "WELL"), "WELL. 01")
  testthat::expect_equal(extract_lasinfo("WELL.  WELL .Abc-01       : WELL", "WELL"), "WELL .Abc-01")
  testthat::expect_equal(extract_lasinfo("WELL . ANY ET AL 12-34-12-34 :WELL", "WELL"), "ANY ET AL 12-34-12-34")
  testthat::expect_equal(extract_lasinfo("WELL. ANY ET AL 12-34-12-34 :WELL", "WELL"), "ANY ET AL 12-34-12-34")
  testthat::expect_equal(extract_lasinfo("WELL . ANY ET AL 12-34-12-34 :WELL", "WELL"), "ANY ET AL 12-34-12-34")
  testthat::expect_equal(extract_lasinfo("WELL . ANY ET 12-34-12-34 :WELL", "WELL"), "ANY ET 12-34-12-34")
  testthat::expect_equal(extract_lasinfo("WELL. A 10-16-39-3 :Well Name", "WELL"), "A 10-16-39-3")
})


# read_las
testthat::test_that("Test read_las with examples las files: log names", {
  testthat::expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example1.las", package = "petroreadr")
    temp <- read_las0(lasfile)
    colnames(temp)
  }, c("WELL", "DEPT", "DT", "RHOB", "NPHI", "SFLU", "SFLA", "ILM", "ILD"))
  testthat::expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example2.las", package = "petroreadr")
    temp <- read_las0(lasfile)
    colnames(temp)
  }, c("WELL", "DEPT", "RHOB", "NPHI", "MSFL", "SFLA", "ILM", "ILD", "SP"))
  testthat::expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example3.las", package = "petroreadr")
    temp <- read_las0(lasfile)
    length(colnames(temp))
  }, 36 + 1) # 36 log tracks + WELL
  testthat::expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example4.las", package = "petroreadr")
    temp <- read_las0(lasfile)
    colnames(temp)
  }, c("WELL", "TIME", "BFR1", "BSG1"))
  testthat::expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example5.las", package = "petroreadr")
    temp <- read_las0(lasfile)
    colnames(temp)
  }, c("WELL", "INDEX.na", "TIME.SEC", "DATE_1.", "DATE_2.", "TIME_1.", "TIME_2.", "TIME_3.", "TPRE.S", "QDPF.KPAA", "BIT.MM"))
})


testthat::test_that("Test write_las and then read_las with examples .las file 1", {
  testthat::expect_equal({
    lasfile <- system.file("extdata", "las2.0_Example1.las", package = "petroreadr")
    temp_1 <- read_las0(lasfile)
    write_las0(temp_1, name = system.file("extdata", "las2.0_Example1_test.las", package = "petroreadr"), verbose = FALSE)
    lasfile <- system.file("extdata", "las2.0_Example1_test.las", package = "petroreadr")
    temp_2 <- read_las0(lasfile)
    colnames(temp_2)
  }, colnames(temp_1))
})
