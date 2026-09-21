suppressMessages({
  # Unit test for Monte Carlo nodes creation
  test_that("create_mcnodes works", {
    # Test that function fails without mctable parameter
    expect_error(create_mcnodes(data = imports_data))

    # Create nodes with both required parameters
    create_mcnodes(data = imports_data, mctable = imports_mctable)

    # Verify dimensions of h_prev match expected values
    expect_equal(dim(h_prev), c(ndvar(), 1, nrow(imports_data)))

    # Compare automatic vs manual node creation
    automatic_node <- round(mean(extractvar(h_prev)), 2)
    manual_node <- round(
      mean(mcstoc(
        runif,
        min = imports_data$h_prev_min[1],
        max = imports_data$h_prev_max[1]
      )),
      2
    )

    # Check if both methods produce same result
    expect_equal(manual_node, automatic_node)

    # Test node creation after setting mctable
    set_mctable(imports_mctable)
    expect_no_error(create_mcnodes(data = imports_data))
    reset_mctable()
  })

  test_that("create_mcnodes handles out-of-order columns correctly", {
    # Create test data with columns in alphabetical order (different from rpert parameter order)
    # rpert expects: min, mode, max
    # Alphabetical order: max, min, mode
    test_data <- data.frame(
      n_animals_max = c(100, 120),
      n_animals_min = c(50, 60),
      n_animals_mode = c(75, 90)
    )

    # Create mctable for rpert distribution
    test_mctable <- data.frame(
      mcnode = "n_animals",
      description = "Number of animals",
      mc_func = "rpert",
      from_variable = NA,
      transformation = NA,
      sensi_analysis = FALSE
    )

    # Create environment for testing
    test_env <- new.env()

    # Create the mcnode - should not error
    expect_no_error(
      result <- create_mcnodes(
        data = test_data,
        mctable = test_mctable,
        envir = test_env
      )
    )
    expect_null(result)

    # Verify the node was created
    expect_true(exists("n_animals", envir = test_env))

    # Verify the created node has correct dimensions
    expect_equal(dim(test_env$n_animals), c(ndvar(), 1, nrow(test_data)))

    # Verify the mcnode does not contain NAs
    expect_false(any(is.na(test_env$n_animals)))
  })

  test_that("matrix_to_mcnodes creates one mcnode per column", {
    X <- matrix(
      c(
        0.1,
        0.2,
        0.3,
        10,
        11,
        12
      ),
      ncol = 2
    )
    colnames(X) <- c("a", "b")
    test_env <- new.env()

    expect_no_error(matrix_to_mcnodes(X, envir = test_env))
    expect_true(exists("a", envir = test_env))
    expect_true(exists("b", envir = test_env))
    expect_equal(dim(test_env$a), c(nrow(X), 1, 1))
    expect_equal(dim(test_env$b), c(nrow(X), 1, 1))
    expect_equal(as.numeric(test_env$a[, 1, 1]), as.numeric(X[, "a"]))
    expect_equal(as.numeric(test_env$b[, 1, 1]), as.numeric(X[, "b"]))

    unnamed_X <- matrix(1:4, ncol = 2)
    unnamed_env <- new.env()
    matrix_to_mcnodes(unnamed_X, envir = unnamed_env)
    expect_true(all(c("x1", "x2") %in% ls(unnamed_env)))
  })

  test_that("matrix_to_mcnodes validates input types", {
    expect_error(matrix_to_mcnodes(1:3), "matrix or data frame")

    X_bad <- data.frame(
      a = c("low", "high"),
      stringsAsFactors = FALSE
    )
    expect_error(matrix_to_mcnodes(X_bad), "must be numeric or logical")

    X_duplicate <- matrix(1:4, ncol = 2)
    colnames(X_duplicate) <- c("a", "a")
    expect_error(matrix_to_mcnodes(X_duplicate), "must be unique")

    X_empty_name <- matrix(1:4, ncol = 2)
    colnames(X_empty_name) <- c("a", "")
    expect_error(matrix_to_mcnodes(X_empty_name), "missing or empty")
  })

  test_that("create_mcnodes safely cleans up skipped inputs", {
    test_data <- data.frame(
      probability_min = c("low", "high"),
      probability_max = c(0.2, 0.3),
      stringsAsFactors = FALSE
    )
    test_mctable <- data.frame(
      mcnode = "probability",
      mc_func = "runif",
      description = "Probability",
      from_variable = NA,
      transformation = NA,
      stringsAsFactors = FALSE
    )

    test_env <- new.env()
    expect_warning(
      result <- create_mcnodes(
        data = test_data,
        mctable = test_mctable,
        envir = test_env
      ),
      "should be numeric or logical"
    )
    expect_null(result)
  })

  test_that("create_mcnodes resolves namespaced distribution functions", {
    test_data <- data.frame(
      probability_min = c(0.1, 0.2),
      probability_max = c(0.3, 0.4)
    )
    test_mctable <- data.frame(
      mcnode = "probability",
      mc_func = "stats::runif",
      description = "Probability",
      from_variable = NA,
      transformation = NA,
      stringsAsFactors = FALSE
    )
    test_env <- new.env()

    expect_no_error(
      create_mcnodes(
        data = test_data,
        mctable = test_mctable,
        envir = test_env
      )
    )
    expect_true(exists("probability", envir = test_env))
    expect_equal(
      dim(test_env$probability),
      c(ndvar(), 1, nrow(test_data))
    )
  })

  test_that("create_mcnodes uses mc2d rpert when freedom masks rpert", {
    if ("package:freedom" %in% search()) {
      skip("freedom is already attached")
    }

    masking_environment <- list2env(list(
      rpert = function(...) {
        stop("The masked function was called")
      }
    ))
    attach(masking_environment, name = "package:freedom")
    on.exit(detach("package:freedom"), add = TRUE)

    test_data <- data.frame(
      probability_min = c(0.1, 0.2),
      probability_mode = c(0.2, 0.3),
      probability_max = c(0.3, 0.4)
    )
    test_mctable <- data.frame(
      mcnode = "probability",
      mc_func = "rpert",
      description = "Probability",
      from_variable = NA,
      transformation = NA,
      stringsAsFactors = FALSE
    )
    test_env <- new.env()

    expect_warning(
      create_mcnodes(
        data = test_data,
        mctable = test_mctable,
        envir = test_env
      ),
      "will use `mc2d::rpert\\(\\)`"
    )
    expect_true(exists("probability", envir = test_env))
  })

  test_that("create_mcnodes supports required distribution parameters", {
    test_data <- data.frame(chi_df = c(2, 4))
    test_mctable <- data.frame(
      mcnode = "chi",
      mc_func = "stats::rchisq",
      description = "Chi-squared value",
      from_variable = NA,
      transformation = NA,
      stringsAsFactors = FALSE
    )
    test_env <- new.env()

    expect_no_error(
      create_mcnodes(
        data = test_data,
        mctable = test_mctable,
        envir = test_env
      )
    )
    expect_true(exists("chi", envir = test_env))
  })

  test_that("create_mcnodes reports unresolved distribution functions", {
    test_data <- data.frame(
      probability_min = c(0.1, 0.2),
      probability_max = c(0.3, 0.4)
    )
    test_mctable <- data.frame(
      mcnode = "probability",
      mc_func = "not_a_distribution",
      description = "Probability",
      from_variable = NA,
      transformation = NA,
      stringsAsFactors = FALSE
    )
    test_env <- new.env()

    expect_error(
      create_mcnodes(
        data = test_data,
        mctable = test_mctable,
        envir = test_env
      ),
      "Distribution function 'not_a_distribution'.*could not be resolved"
    )
    expect_false(any(c(
      "probability_min",
      "probability_max"
    ) %in% ls(test_env)))
  })

  test_that("create_mcnodes retries stochastic nodes after NA removal", {
    test_data <- data.frame(
      probability_min = c(0.1, NA),
      probability_max = c(0.3, 0.4)
    )
    test_mctable <- data.frame(
      mcnode = "probability",
      mc_func = "runif",
      description = "Probability",
      from_variable = NA,
      transformation = NA,
      stringsAsFactors = FALSE
    )
    test_env <- new.env()

    expect_no_error(
      create_mcnodes(
        data = test_data,
        mctable = test_mctable,
        envir = test_env
      )
    )
    expect_true(exists("probability", envir = test_env))
    expect_false(any(is.na(test_env$probability)))
  })
})
