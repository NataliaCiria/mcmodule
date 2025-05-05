suppressMessages({

  # Unit tests for add_group_id function
  test_that("add_group_id single dataframe works", {
    # Create test data
    test_df <- data.frame(
      category = rep(c("A", "B"), 5),
      value = rnorm(10),
      type = rep(c("X", "Y"), each = 5)
    )

    # Test with one key
    result <- add_group_id(test_df, by="category")
    expect_equal(max(result$g_id), 2)  # Should have 2 groups

    # Test with two keys
    result <- add_group_id(test_df, by=c("category","type"))
    expect_equal(max(result$g_id), 4)  # Should have 4 groups

  })

  test_that("add_group_id two dataframes works", {
    # Create test data
    df1 <- data.frame(
      category = rep(c("A", "B"), 5),
      value = rnorm(10),
      type = rep(c("X", "Y"), each = 5)
    )

    df2 <- data.frame(
      category = rep(c("A", "B"), 5),
      value = rnorm(10),
      type = rep(c("Y", "X"), each = 5)
    )

    # Test with two dataframes
    result <- add_group_id(df1, df2, by=c("category","type"))
    expect_equal(names(result), c("x", "y"))
    expect_equal(
      unique(result$x[order(result$x$g_id),c("g_id","category","type")]),
      unique(result$y[order(result$y$g_id),c("g_id","category","type")]))

    # Test automatic categorical variable detection
    result_auto <- add_group_id(df1, df2)
    expect_equal(
      unique(result_auto$x[order(result_auto$x$g_id),c("g_id","category","type")]),
      unique(result_auto$y[order(result_auto$y$g_id),c("g_id","category","type")]))

    # Test error handling
    expect_error(add_group_id(df1, df2, by="nonexistent"))
  })

  test_that("mc_keys for keys works", {
    # Create mock module
    mock_module <- list(
      node_list = list(
        test_node = list(
          data_name = "test_data",
          keys = c("key1", "key2")
        )
      ),
      data = list(
        test_data = data.frame(
          key1 = c("A", "B"),
          key2 = c(1, 2),
          value = c(10, 20)
        )
      )
    )

    # Test with specified keys
    result <- mc_keys(mock_module, "test_node", c("key1", "key2"))
    expect_equal(ncol(result), 3)  # scenario_id + 2 keys
    expect_true(all(c("scenario_id", "key1", "key2") %in% names(result)))

    # Test default scenario_id
    expect_true(all(result$scenario_id == "0"))

    # Test with missing keys
    expect_error(
      mc_keys(mock_module, "test_node", c("key1", "nonexistent")),
      "Columns nonexistent not found"
    )

    # Test with invalid node name
    expect_error(
      mc_keys(mock_module, "nonexistent_node"),
      "Node not found in module"
    )
  })

  test_that("mc_keys for agg_keys works", {
    # Create mock module with aggregated node
    mock_agg_module <- list(
      node_list = list(
        agg_node = list(
          agg_keys = c("group"),
          summary = data.frame(
            group = c("X", "Y"),
            count = c(5, 10)
          )
        )
      )
    )

    result <- mc_keys(mock_agg_module, "agg_node")
    expect_equal(ncol(result), 2)  # scenario_id + group
    expect_true(all(c("scenario_id", "group") %in% names(result)))
  })


  test_that("mc_match handles basic matching correctly", {
    # Create mock module with mcnodes
    mock_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(1, 2, 3), type="0", nvariates = 3),
                          max=mcdata(c(2, 3, 4), type="0", nvariates = 3),
                          nvariates = 3),
          data_name = "data_x"
        ),
        node_y = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(5, 6, 7), type="0", nvariates = 3),
                          max=mcdata(c(6, 7, 8), type="0", nvariates = 3),
                          nvariates = 3),
          data_name = "data_y"
        )
      ),
      data = list(
        data_x = data.frame(
          key = c("A", "B", "C"),
          scenario_id = c("0", "1", "1")
        ),
        data_y = data.frame(
          key = c("B", "C", "D"),
          scenario_id = c("0", "1", "1")
        )
      )
    )

    # Test basic functionality
    result <- mc_match(mock_module, "node_x", "node_y", keys_names = "key")

    expect_type(result, "list")
    expect_length(result, 3)
    expect_true(all(c("node_x_match", "node_y_match", "index") %in% names(result)))

    # Test error handling
    expect_error(
      mc_match(mock_module, "nonexistent", "node_y"),
      "Nodes nonexistent not found"
    )

    expect_error(
      mc_match(list(), "node_x", "node_y"),
      "Invalid mcmodule structure"
    )
  })

})
