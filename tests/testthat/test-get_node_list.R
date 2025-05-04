test_that("get_node_list works", {
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
    dataset1 = list(
      data = data.frame(
        x = letters[1:3],
        input_a = 1:3,
        input_b = 4:6
      ),
      keys = c("x")
    )
  )

  # Run function
  node_list <- get_node_list(
    model_exp = test_exp,
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

  # Test error cases
  expect_error(get_node_list("not an expression"))
  expect_error(get_node_list(quote(not_valid <- 1)))
})
