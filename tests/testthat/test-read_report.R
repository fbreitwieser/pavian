write_ref_output <- FALSE

test_that("kraken input works", {
  res <- read_report("read_report/input/kraken-report.txt")
  if (write_ref_output)
    saveRDS(res, "read_report/ref_output/kraken-report-res.rds")
  
  expect_equal(res, readRDS("read_report/ref_output/kraken-report-res.rds"))
})

test_that("krakenuniq input works", {
  res <- read_report("read_report/input/krakenuniq-report.txt")
  if (write_ref_output)
    saveRDS(res, "read_report/ref_output/krakenuniq-report-res.rds")
  
  expect_equal(res, readRDS("read_report/ref_output/krakenuniq-report-res.rds"))
})


test_that("kraken2uniq input works", {
  res <- read_report("read_report/input/k2uniq-report.txt")
  if (write_ref_output)
    saveRDS(res, "read_report/ref_output/k2uniq-report-res.rds")
  
  expect_equal(res, readRDS("read_report/ref_output/k2uniq-report-res.rds"))
})

test_that("metaphlan2 input works", {
  res <- read_report("read_report/input/metaphlan2-report.txt")
  if (write_ref_output)
    saveRDS(res, "read_report/ref_output/metaphlan2-report-res.rds")
  
  expect_equal(res, readRDS("read_report/ref_output/metaphlan2-report-res.rds"))
})


test_that("metaphlan3 input works", {
  res <- read_report("read_report/input/metaphlan3-report.txt")
  if (write_ref_output)
    saveRDS(res, "read_report/ref_output/metaphlan3-report-res.rds")
  
  expect_equal(res, readRDS("read_report/ref_output/metaphlan3-report-res.rds"))
})

test_that("metaphlan4 input works", {
  res <- read_report("read_report/input/metaphlan4-report.txt")
  if (write_ref_output)
    saveRDS(res, "read_report/ref_output/metaphlan4-report-res.rds")
  
  expect_equal(res, readRDS("read_report/ref_output/metaphlan4-report-res.rds"))
})
