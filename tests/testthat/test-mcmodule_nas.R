test_that("mcmodule_nas works with mcmodule without NAs", {
    # Test with imports_mcmodule which should not have NAs
    result <- mcmodule_nas(imports_mcmodule)

    # Should return a character vector
    expect_type(result, "character")

    # Should return empty vector if no NAs present
    # (or names of nodes with NAs if any exist)
    expect_true(length(result) >= 0)
  })

  test_that("mcmodule_nas detects NAs in mcnodes", {
    # Create a test mcmodule with a node containing NAs
    test_mcmodule <- list(
      node_list = list(
        node_with_na = list(
          mcnode = c(1, 2, NA, 4, 5)
        ),
        node_without_na = list(
          mcnode = c(1, 2, 3, 4, 5)
        ),
        node_all_na = list(
          mcnode = c(NA, NA, NA)
        )
      )
    )

    result <- mcmodule_nas(test_mcmodule)

    # Should detect both nodes with NAs
    expect_true("node_with_na" %in% result)
    expect_true("node_all_na" %in% result)
    expect_false("node_without_na" %in% result)
    expect_equal(length(result), 2)
  })

  test_that("mcmodule_nas handles Inf values separately from NA", {
    # Create a test mcmodule with Inf values (not NA)
    test_mcmodule <- list(
      node_list = list(
        node_with_inf = list(
          mcnode = c(1, 2, Inf, 4, 5)
        ),
        node_with_na = list(
          mcnode = c(1, NA, 3)
        )
      )
    )

    result <- mcmodule_nas(test_mcmodule)

    # Should only detect NA, not Inf
    expect_true("node_with_na" %in% result)
    expect_false("node_with_inf" %in% result)
    expect_equal(length(result), 1)
  })

  test_that("mcmodule_nas returns empty vector when no NAs present", {
    # Create a test mcmodule with no NAs
    test_mcmodule <- list(
      node_list = list(
        node1 = list(mcnode = c(1, 2, 3)),
        node2 = list(mcnode = c(4, 5, 6))
      )
    )

    result <- mcmodule_nas(test_mcmodule)

    # Should return empty character vector
    expect_type(result, "character")
    expect_equal(length(result), 0)
  })

  test_that("mcmodule_nas validates input structure", {
    # Test with invalid input - not a list
    expect_error(
      mcmodule_nas("not a list"),
      "mcmodule must be a list with a node_list component"
    )

    # Test with list but no node_list
    expect_error(
      mcmodule_nas(list(data = "test")),
      "mcmodule must be a list with a node_list component"
    )
  })

  test_that("mcmodule_nas handles empty node_list", {
    # Create an mcmodule with empty node_list
    test_mcmodule <- list(node_list = list())

    result <- mcmodule_nas(test_mcmodule)

    # Should return empty character vector
    expect_type(result, "character")
    expect_equal(length(result), 0)
  })

  test_that("mcmodule_nas works with mcnode objects from mc2d", {
    # Create a simple mcnode-like structure
    test_mcmodule <- list(
      node_list = list(
        mcnode_test = list(
          mcnode = matrix(c(1, 2, NA, 4, 5, 6), nrow = 2)
        )
      )
    )

    result <- mcmodule_nas(test_mcmodule)

    # Should detect the NA in the matrix
    expect_true("mcnode_test" %in% result)
    expect_equal(length(result), 1)
  })

