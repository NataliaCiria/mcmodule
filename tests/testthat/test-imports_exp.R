test_that("imports_exp works", {
  create_mc_nodes(data = imports_data, mctable = imports_mctable)
  eval(imports_exp)
  expect_true(is.mcnode(no_detect_a))
})
