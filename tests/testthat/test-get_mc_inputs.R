test_that("get_mc_inputs works", {
  expect_equal(length(get_mc_inputs()), 0)
  set_mctable(imports_mctable)
  set_data_keys(imports_data_keys)
  expect_equal(names(get_mc_inputs()), names(imports_data_keys))
  reset_data_keys()
  reset_mctable()
})
