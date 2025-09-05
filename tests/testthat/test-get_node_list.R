suppressMessages({
  test_that("get_node_list works (basic use)", {
    # Create test data
    test_exp <- quote({
      result <- input_a * input_b
      final <- result + prev_value
    })

    test_mctable <- data.frame(
      mcnode = c("input_a", "input_b"),
      mc_func = c("runif", "rnorm"),
      description = c("Test input A", "Test input B")
    )

    test_data_keys <- list(
      test_data=list(
      cols = c("x", "input_a_min","input_a_max","input_b_mean","input_b_sd"),
      keys = c("x"))
    )

    # Run function
    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    # Test structure
    expect_s3_class(node_list, "mcnode_list")

    # Test input nodes
    expect_true("input_a" %in% names(node_list))
    expect_true("input_b" %in% names(node_list))
    expect_equal(node_list$input_a$type, "in_node")
    expect_equal(node_list$input_b$type, "in_node")

    # Test output nodes
    expect_true("result" %in% names(node_list))
    expect_true("final" %in% names(node_list))
    expect_equal(node_list$result$type, "out_node")
    expect_equal(node_list$final$type, "out_node")

    # Test relationships
    expect_equal(sort(node_list$result$inputs), sort(c("input_a", "input_b")))
    expect_equal(sort(node_list$final$inputs), sort(c("result", "prev_value")))

    # Test keys
    expect_equal(node_list$result$keys, "x")

    # Test error cases
    expect_error(get_node_list("not an expression"))
    expect_error(get_node_list(quote(not_valid <- 1)))
  })

  test_that("get_node_list works for transformed inputs", {
    # Create test data
    test_exp <- quote({
      result <- input_a * input_b_yes
      final <- result + 1
    })

    test_mctable <- data.frame(
      mcnode = c("input_a", "input_b_yes"),
      mc_func = c("runif", NA),
      from_variable = c(NA, "input_b"),
      transformation = c(NA, "value=='yes'"),
      description = c("Test input A", "Test input B")
    )

    test_data_keys <- list(
      test_data=list(
        cols = c("x", "input_a_min","input_a_max","input_b"),
        keys = c("x"))
    )

    # Run function
    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    expect_equal(node_list$input_b_yes$type, "in_node")
    expect_equal(node_list$input_b_yes$keys, "x")
    expect_equal(node_list$input_b_yes$inputs_col, "input_b")
    expect_equal(node_list$input_b_yes$input_dataset, "test_data")


  })

  test_that("get_node_list works with several data tables", {
    # Create test data
    test_exp <- quote({
      result <- input_a * input_b * input_c
      final <- result + prev_value
    })

    test_mctable <- data.frame(
      mcnode = c("input_a", "input_b","input_c"),
      mc_func = c("runif", "rnorm",NA),
      description = c("Test input A", "Test input B", "Test input C")
    )

    test_data_keys <- list(
      test_data_x=list(
        cols = c("x", "input_a_min","input_a_max","input_b_mean","input_b_sd"),
        keys = c("x")),
      test_data_y=list(
        cols = c("y", "input_c"),
        keys = c("y"))
    )

    # Run function
    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    # Test keys
    expect_equal(node_list$result$keys,c("x","y"))

  })
})
