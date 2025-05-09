suppressMessages({
  test_that("mc_keys for keys works", {
    # Create mock module
    test_module <- list(
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
    result <- mc_keys(test_module, "test_node", c("key1", "key2"))
    expect_equal(ncol(result), 3) # scenario_id + 2 keys
    expect_true(all(c("scenario_id", "key1", "key2") %in% names(result)))

    # Test default scenario_id
    expect_true(all(result$scenario_id == "0"))

    # Test with missing keys
    expect_error(
      mc_keys(test_module, "test_node", c("key1", "nonexistent")),
      "Columns nonexistent not found"
    )

    # Test with invalid node name
    expect_error(
      mc_keys(test_module, "nonexistent_node"),
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
    expect_equal(ncol(result), 2) # scenario_id + group
    expect_true(all(c("scenario_id", "group") %in% names(result)))
  })

  test_that("mc_match group matching works", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcstoc(runif,
            min = mcdata(c(1, 2, 3), type = "0", nvariates = 3),
            max = mcdata(c(2, 3, 4), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(runif,
            min = mcdata(c(5, 6, 7), type = "0", nvariates = 3),
            max = mcdata(c(6, 7, 8), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_y",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B", "C")
        ),
        data_y = data.frame(
          category = c("B", "C", "A")
        )
      )
    )

    result <- mc_match(test_module, "node_x", "node_y")

    # Test dimensions
    expect_equal(dim(result$node_x_match), dim(test_module$node_list$node_x$mcnode))
    expect_equal(dim(result$node_y_match), dim(test_module$node_list$node_y$mcnode))

    # Test that categories are matched correctly
    expect_equal(result$keys_xy$category, test_module$data$data_x$category)

    # Verify expected keys_xy
    expect_equal(result$keys_xy$category, c("A", "B", "C"))
    expect_equal(result$keys_xy$scenario_id, c("0", "0", "0"))
  })

  test_that("mc_match scenario matching works", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcstoc(runif,
            min = mcdata(c(1, 2, 3, 4), type = "0", nvariates = 4),
            max = mcdata(c(2, 3, 4, 5), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(runif,
            min = mcdata(c(5, 6, 7, 8), type = "0", nvariates = 4),
            max = mcdata(c(6, 7, 8, 9), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "data_y",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1")
        ),
        data_y = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "2", "2")
        )
      )
    )

    result <- mc_match(test_module, "node_x", "node_y")

    # Check scenario matching logic
    expect_equal(nrow(result$keys_xy), 6)

    # Verify dimensions of matched nodes
    expect_equal(dim(result$node_x_match)[2], dim(result$node_y_match)[2])

    # Verify expected keys_xy
    expect_equal(result$keys_xy$category, c("A", "B", "A", "B", "A", "B"))
    expect_equal(result$keys_xy$scenario_id, c("0", "0", "1", "1", "2", "2"))
  })


  test_that("mc_match null matching works", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcstoc(runif,
            min = mcdata(c(1, 2, 3), type = "0", nvariates = 3),
            max = mcdata(c(2, 3, 4), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(runif,
            min = mcdata(c(5, 6, 7), type = "0", nvariates = 3),
            max = mcdata(c(6, 7, 8), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_y",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B", "C"),
          scenario_id = c("0", "0", "0")
        ),
        data_y = data.frame(
          category = c("B", "B", "B"),
          scenario_id = c("0", "1", "2")
        )
      )
    )

    result <- mc_match(test_module, "node_x", "node_y")

    # Verify dimensions of matched nodes
    expect_equal(dim(result$node_x_match)[2], dim(result$node_y_match)[2])

    # Verify expected keys_xy
    expect_equal(result$keys_xy$category, c("A", "B", "C", "B", "B"))
    expect_equal(result$keys_xy$scenario_id, c("0", "0", "0", "1", "2"))
  })

  test_that("wif_match works", {
    # Test data
    x <- data.frame(
      category = c("a", "b", "a", "b"),
      scenario_id = c(0, 0, 1, 1),
      hg = c(1, 2, 1, 2),
      value = 1:4
    )

    y <- data.frame(
      category = c("a", "b", "a", "b"),
      scenario_id = c(0, 0, 2, 2),
      hg = c(1, 2, 1, 2),
      value = 5:8
    )

    # Automatic matching
    result <- wif_match(x, y)
    expect_equal(result$x$scenario_id, result$y$scenario_id)
    expect_equal(result$x$scenario_id, c(0, 0, 1, 1, 2, 2))
    expect_equal(result$x$hg, result$y$hg)
    expect_equal(result$x$hg, c(1, 2, 1, 2, 1, 2))

    # Match by type
    result_by <- wif_match(x, y, "category")
    expect_equal(result_by$x$scenario_id, result_by$y$scenario_id)
    expect_equal(result_by$x$hg, result_by$y$hg)

    # Test error on unmatched groups
    y_bad <- data.frame(
      category = c("a", "c", "a", "c"),
      scenario_id = c(0, 0, 2, 2),
      hg = c(1, 3, 1, 3),
      value = 5:8
    )
    expect_error(wif_match(x, y_bad), "Groups not found")
    expect_error(wif_match(x, y_bad, "category"), "Groups not found")
  })
})
