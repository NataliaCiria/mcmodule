suppressMessages({
  # Tests for mcmodule_dim_check
  test_that("mcmodule_dim_check returns correct dimensions", {
    result <- mcmodule_dim_check(imports_mcmodule)

    expect_type(result, "list")
    expect_equal(result$n_mcnodes, 7)
    expect_equal(result$n_variate, 6)
    expect_equal(result$n_uncertainty, 1001)
  })

  test_that("mcmodule_dim_check works with subset of nodes", {
    result <- mcmodule_dim_check(
      imports_mcmodule,
      mc_names = c("w_prev", "test_origin")
    )

    expect_equal(result$n_mcnodes, 2)
  })

  test_that("mcmodule_dim_check errors on mismatched variate dimensions", {
    mock_mcmodule <- imports_mcmodule

    mock_mcmodule$node_list$wrong_node <- list(
      mcnode = mcdata(1, type = "0", nvariates = 2)
    )

    expect_error(
      mcmodule_dim_check(mock_mcmodule),
      "same number of variate simulations"
    )
  })

  test_that("mcmodule_dim_check errors on mismatched uncertainty dimensions", {
    mock_mcmodule <- imports_mcmodule

    mock_mcmodule$node_list$wrong_node <- list(
      mcnode = mcstoc(runif, min = 0, max = 1, nvariates = 6, nsv = 100)
    )
    expect_error(
      mcmodule_dim_check(mock_mcmodule),
      "same number of uncertanty simulations"
    )
  })

  # Tests for mcmodule_to_matrices
  test_that("mcmodule_to_matrices returns correct structure", {
    result <- mcmodule_to_matrices(imports_mcmodule)

    expect_type(result, "list")
    expect_length(result, 6)
    expect_true(all(sapply(result, is.matrix)))
    expect_equal(nrow(result[[1]]), 1001)
    expect_equal(ncol(result[[1]]), length(imports_mcmodule$node_list))
  })

  test_that("mcmodule_to_matrices works with single variate", {
    mock_mcmodule <- imports_mcmodule

    mock_mcmodule$node_list$uni_variate <- list(
      mcnode = mcstoc(runif, min = 0, max = 1)
    )

    result <- mcmodule_to_matrices(mock_mcmodule)

    expect_length(result, 6)
    expect_equal(nrow(result[[1]]), 1001)
    expect_equal(ncol(result[[1]]), length(mock_mcmodule$node_list))
  })

  test_that("mcmodule_to_matrices handles list of mcnodes", {
    result <- mcmodule_to_matrices(
      imports_mcmodule,
      mc_names = c("w_prev", "test_origin")
    )
    expect_equal(ncol(result[[1]]), 2)
    expect_true(all(result[[1]][, 2] == 0.5))
  })

  # Tests for mcmodule_to_mc
  test_that("mcmodule_to_mc returns list of mc objects", {
    result <- mcmodule_to_mc(imports_mcmodule)

    expect_type(result, "list")
    expect_length(result, 6)
    expect_s3_class(result[[1]], "mc")
    expect_true(all(names(imports_mcmodule$node_list) %in% names(result[[1]])))
  })

  test_that("mcmodule_to_mc works with subset of nodes", {
    result <- mcmodule_to_mc(
      imports_mcmodule,
      mc_names = c("w_prev", "test_origin")
    )
    expect_length(result[[1]], 2)
  })

  # Tests for mcmodule_index
  test_that("mcmodule_index returns correct structure", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_index(test_module)

    expect_type(result, "list")
    expect_named(result, c("module_exp_data", "data_keys", "global_keys"))
    expect_true("variate" %in% names(result$data_keys))
    expect_true("data_name" %in% names(result$data_keys))
    expect_true(nrow(result$data_keys) == 6)
    expect_equal(result$global_keys, c("pathogen", "origin"))
  })

  # Tests for mcmodule_corr
  test_that("mcmodule_corr works with one expression", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_corr(test_module)

    expect_s3_class(result, "data.frame")

    # Check required columns
    expect_true(all(
      c(
        "exp",
        "exp_n",
        "variate",
        "output",
        "input",
        "value",
        "method",
        "use"
      ) %in%
        names(result)
    ))

    # Check key columns are included
    expect_true(all(c("pathogen", "origin") %in% names(result)))

    # Check output column values
    expect_true(all(result$output == "no_detect_a"))

    # Check method values (default is spearman, kendall, pearson)
    expect_true(all(result$method %in% c("spearman", "kendall", "pearson")))

    # Check exp column
    expect_true(all(result$exp == "imports"))

    # Check variate range (should be 1 to 6 for imports_data)
    expect_true(all(result$variate %in% 1:6))

    # Check inputs (should include w_prev and test_sensi)
    expect_true(all(c("w_prev", "test_sensi") %in% unique(result$input)))

    # Check number of rows: 6 variates × 2 inputs = 36 rows
    expect_equal(nrow(result), 12)
  })

  test_that("mcmodule_corr works with multiple expressions", {
    #  Create previous_module
    previous_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    previous_module <- trial_totals(
      previous_module,
      mc_names = "no_detect_a",
      trials_n = "animals_n",
      subsets_n = "farms_n",
      subsets_p = "h_prev",
      mctable = imports_mctable
    )

    #  Create current_module
    current_data <- data.frame(
      pathogen = c("a", "a", "a", "b", "b", "b", "b"),
      origin = c("east", "south", "nord", "east", "south", "nord", "nord"),
      clean = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
      scenario_id = c("0", "0", "0", "0", "0", "0", "clean_transport"),
      survival_p_min = c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7),
      survival_p_max = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
    )

    current_data_keys <- list(
      survival = list(
        cols = c("pathogen", "survival_p_min", "survival_p_max"),
        keys = c("pathogen")
      )
    )

    current_mctable <- data.frame(
      mcnode = c("clean", "survival_p"),
      description = c("Transport cleaned", "Survival probability"),
      mc_func = c(NA, "runif"),
      from_variable = c(NA, NA),
      transformation = c(NA, NA),
      sensi_analysis = c(TRUE, TRUE)
    )

    current_exp <- quote({
      imported_contaminated <- no_detect_a_set * survival_p * (1 - clean)
    })

    current_module <- eval_module(
      exp = c(current = current_exp),
      data = current_data,
      mctable = current_mctable,
      data_keys = current_data_keys,
      prev_mcmodule = previous_module
    )

    combined_module <- combine_modules(previous_module, current_module)

    combined_module <- at_least_one(
      combined_module,
      c("no_detect_a", "imported_contaminated"),
      name = "total"
    )

    result <- mcmodule_corr(combined_module)

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(
      c("exp", "variate", "input", "value", "output") %in% names(result)
    ))
    result <- mcmodule_corr(combined_module, output = "total")
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(result$output == "total"))

    result <- mcmodule_corr(combined_module, by_exp = TRUE)
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(result$exp %in% c("imports", "current")))
  })
})
