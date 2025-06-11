suppressMessages({
  # Unit test for Monte Carlo table setting and resetting
  test_that("set_mctable and reset_mctable work", {
    # Verify mctable starts empty
    expect_equal(nrow(set_mctable()), 0)

    # Test that function fails with wrong input
    expect_error(set_mctable(imports_data))

    # Set mctable and verify it matches expected value
    set_mctable(imports_mctable)
    expect_equal(set_mctable(), imports_mctable)

    # Verify mctable reset
    reset_mctable()
    expect_equal(nrow(set_mctable()), 0)
  })
})
