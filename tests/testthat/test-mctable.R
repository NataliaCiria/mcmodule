test_that("mctable creation works", {
  expect_equal(nrow(mctable()),0)
  expect_error(mctable(imports_data))
  mctable(imports_mctable)
  expect_equal(mctable(),imports_mctable)
})
