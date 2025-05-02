test_that("mcnodes creation works", {
  expect_error(create_mc_nodes(data = imports_data))

  create_mc_nodes(data = imports_data, mctable = imports_mctable)
  expect_equal(2 * 2, 4)
})
