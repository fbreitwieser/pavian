write_ref_output <- FALSE

test_that("kraken2uniq input works", {
  res <- read_report("read_report/input/k2uniq-report.txt")
  if (write_ref_output)
    saveRDS(res, "read_report/ref_output/k2uniq-report-res.rds")
  
  expect_equal(res, readRDS("read_report/ref_output/k2uniq-report-res.rds"))
})

