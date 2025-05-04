# Unit test for data model setting
test_that("set_data_keys works", {
  # Verify data model starts empty
  expect_equal(length(set_data_keys()), 0)

  # Test that function fails with wrong input
  expect_error(set_data_keys("not a list"))
  expect_error(set_data_keys(list(bad_element = "not proper structure")))

  # Set data model and verify it matches expected value
  test_model <- list(
    test_data = list(
      data = data.frame(x = letters[1:3], y = 1:3),
      keys = c("x")
    )
  )
  set_data_keys(test_model)
  expect_equal(set_data_keys(), test_model)

  # Reset and verify it's empty
  reset_data_keys()
  expect_equal(length(set_data_keys()), 0)
  expect_type(set_data_keys(), "list")
})
