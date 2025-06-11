suppressMessages({
  # Unit test for data model setting
  test_that("set_data_keys works", {
    # Verify data model starts empty
    expect_equal(length(set_data_keys()), 0)

    # Test that function fails with wrong input
    expect_error(set_data_keys("not a list"))
    expect_error(set_data_keys(list(bad_element = "not proper structure")))

    # Set data model and verify it matches expected value
    test_data_keys <- list(
      test_data = list(
        data = data.frame(x = letters[1:3], y = 1:3),
        keys = c("x")
      )
    )
    expect_message(set_data_keys(test_data_keys),"data_keys set to test_data_keys")
    expect_equal(set_data_keys(), test_data_keys)

    # Reset and verify it's empty
    reset_data_keys()
    expect_equal(length(set_data_keys()), 0)
    expect_type(set_data_keys(), "list")
  })
})
