suppressMessages({
  test_that("mc_summary works", {
    test_module <- list(
      node_list = list(
        p1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
            max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C"),
          scenario_id = c("0", "0", "0")
        )
      )
    )

    # Test basic summary from mcmodule
    result <- mc_summary(test_module, "p1")
    expect_true(all(c("category", "scenario_id") %in% names(result)))

    # Test basic summary from data
    result_data <- mc_summary(
      data = test_module$data$test_data,
      mcnode = test_module$node_list$p1$mcnode
    )
    expect_true("variate" %in% names(result_data))

    # Test with digits parameter
    result_rounded <- mc_summary(test_module, "p1", digits = 2)
    expect_true(all(sapply(
      result_rounded[sapply(result_rounded, is.numeric)],
      function(x) all(abs(x - round(x, 2)) < 1e-10)
    )))

    # Test keys from mcmodule
    result_keys <- mc_summary(test_module, "p1", keys_names = c("category"))
    expect_true("category" %in% names(result_keys))

    # Test keys from data
    result_data_keys <- mc_summary(
      data = test_module$data$test_data,
      mcnode = test_module$node_list$p1$mcnode,
      keys_names = c("category")
    )
    expect_true("category" %in% names(result_data_keys))

    # Requested key order is preserved
    ordered_keys <- mc_summary(
      test_module,
      "p1",
      keys_names = c("scenario_id", "category")
    )
    expect_equal(names(ordered_keys)[2:3], c("scenario_id", "category"))

    # Test errors
    expect_error(mc_summary(test_module, "nonexistent_node"))
    expect_error(mc_summary(test_module$data, "p1"))
    expect_error(mc_summary(
      test_module,
      "p1",
      keys_names = c("nonexistent_key")
    ))
    expect_error(
      mc_summary(data = test_module$data$test_data),
      "mcnode must be provided"
    )
    expect_error(mc_summary(test_module, "p1", digits = 0), "positive integer")
    expect_error(mc_summary(test_module, "p1", sep_keys = NA), "TRUE or FALSE")
    expect_error(
      mc_summary(
        data = test_module$data$test_data[1:2, ],
        mcnode = test_module$node_list$p1$mcnode
      ),
      "data has 2 rows but mcnode has 3 variates"
    )
  })

  test_that("mc_summary works with mc_filter nodes", {
    # Create test module with filtered nodes
    test_module <- list(
      node_list = list(
        p1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3, 0.4), type = "0", nvariates = 4),
            max = mcdata(c(0.2, 0.3, 0.4, 0.5), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("category", "region")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A", "B"),
          region = c("North", "North", "South", "South"),
          scenario_id = c("0", "0", "0", "0")
        )
      )
    )

    # Create filtered node (category == "A")
    filtered_module <- mc_filter(
      test_module,
      "p1",
      category == "A",
      name = "p1_A"
    )

    # Test summary on filtered node
    result <- mc_summary(filtered_module, "p1_A")
    expect_true(is.data.frame(result))
    expect_true(all(c("category", "region") %in% names(result)))
    expect_equal(nrow(result), 2) # Two "A" categories
    expect_true(all(result$category == "A"))
  })

  test_that("mc_summary works with mc_compare nodes", {
    # Create test module with comparison
    test_module <- list(
      node_list = list(
        p1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.1, 0.2), type = "0", nvariates = 4),
            max = mcdata(c(0.2, 0.3, 0.2, 0.3), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1")
        )
      )
    )

    # Create comparison node
    compared_module <- mc_compare(
      test_module,
      "p1",
      baseline = "0",
      type = "difference",
      name = "p1_diff"
    )

    # Test summary on compared node
    result <- mc_summary(compared_module, "p1_diff")
    expect_true(is.data.frame(result))
    expect_true(all(c("mean", "sd") %in% names(result)))
    expect_equal(nrow(result), 2) # Two what-if variates
    # Verify only what-if scenarios in summary (not baseline "0")
    if ("scenario_id" %in% names(result)) {
      expect_false(any(result$scenario_id == "0"))
    }
  })

  test_that("mc_summary applies options to pre-calculated summaries", {
    stored_summary <- data.frame(
      mc_name = c("risk_agg", "risk_agg"),
      scenario_id = c("0", "1"),
      category_code = c(101, 102),
      mean = c(0.12345, 0.98765),
      sd = c(0.01234, 0.04321),
      stringsAsFactors = FALSE
    )
    test_module <- list(
      node_list = list(
        risk_agg = list(
          mcnode = mcdata(c(0.1, 0.9), type = "0", nvariates = 2),
          type = "agg_total",
          keys = c("category_code"),
          agg_keys = c("scenario_id", "category_code"),
          summary = stored_summary
        )
      ),
      data = list()
    )

    result <- mc_summary(
      test_module,
      "risk_agg",
      sep_keys = FALSE,
      digits = 2
    )

    expect_equal(result$keys, c("0, 101", "1, 102"))
    expect_equal(result$mean, c(0.12, 0.99))
    expect_equal(result$sd, c(0.01, 0.04))
    expect_false(any(c("scenario_id", "category_code") %in% names(result)))

    # Formatting must not modify the summary stored in the module.
    expect_equal(test_module$node_list$risk_agg$summary, stored_summary)
  })

  test_that("signif_round handles negative values by magnitude", {
    expect_equal(
      signif_round(c(-0.001234, -1.234), digits = 2),
      c(-0.0012, -1.23)
    )
  })

})
