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
  expect_equal(nodes$result$type, "out_node")
  expect_equal(nodes$final$type, "out_node")

  # Test relationships
  expect_equal(sort(node_list$result$inputs), sort(c("input_a", "input_b")))
  expect_equal(sort(node_list$final$inputs), sort(c("result", "prev_value")))

  # Test error cases
  expect_error(get_node_list("not an expression"))
  expect_error(get_node_list(quote(not_valid <- 1)))
})

test_that("get_previous_nodes works", {
  # Create test data
  test_node <- list(mcnode = "test_value")
  test_node_list <- list(
    node1 = list(mcnode = "value1"),
    node2 = list(mcnode = "value2")
  )
  class(test_node_list) <- "mcnode_list"

  test_mcmodule <- list(node_list = test_node_list)
  class(test_mcmodule) <- "mcmodule"

  # Test with mcmodule object
  result1 <- get_previous_nodes(test_mcmodule, c("node1"))
  expect_equal(length(result1), 1)
  expect_equal(result1$node1$mcnode, "value1")

  # Test with mcnode_list object
  result2 <- get_previous_nodes(test_node_list, c("node2"))
  expect_equal(length(result2), 1)
  expect_equal(result2$node2$mcnode, "value2")

  # Test with invalid input
  expect_error(get_previous_nodes("invalid"))

  # Test with non-existent nodes
  result3 <- get_previous_nodes(test_mcmodule, c("non_existent"))
  expect_equal(length(result3), 0)

  # Test with no nodes specified
  result4 <- get_previous_nodes(test_mcmodule)
  expect_equal(length(result4), 0)
})
