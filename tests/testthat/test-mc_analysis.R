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
    expect_true("variate" %in% names(result))
    expect_true("exp" %in% names(result))
    expect_true(all(
      c("w_prev", "test_sensi", "pathogen", "origin") %in% names(result)
    ))
    expect_true(all(result$output == "no_detect_a"))
    expect_true(nrow(result) == 6)
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
      survival_p_min = c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.1),
      survival_p_max = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.15)
    )

    current_data_keys <- list(
      survival = list(
        cols = c("pathogen", "clean", "survival_p_min", "survival_p_max"),
        keys = c("pathogen", "clean")
      )
    )

    current_mctable <- data.frame(
      mcnode = c("survival_p"),
      description = c("Survival probability"),
      mc_func = c("runif"),
      from_variable = c(NA),
      transformation = c(NA),
      sensi_analysis = c(FALSE)
    )

    current_exp <- quote({
      imported_contaminated <- no_detect_a_set * survival_p
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
  })

  test_that("mcmodule_corr includes key columns in output", {
    skip_if_not_installed("mc2d")

    mcmodule <- create_mock_mcmodule(
      n_uncertainty = 100,
      n_variate = 2,
      n_nodes = 3
    )
    mcmodule$node_list$output <- list(
      mcnode = mcdata(rnorm(100 * 2), type = "VU", nvariates = 2),
      module = "exp1",
      type = "out_node"
    )

    result <- mcmodule_corr(mcmodule, output = "output")

    expect_true("key1" %in% names(result) || "key2" %in% names(result))
  })
})
