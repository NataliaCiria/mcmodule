suppressMessages({
  test_that("set_sample_design and reset_sample_desing work", {
    reset_sample_desing()
    expect_null(set_sample_design())

    X <- data.frame(
      a = c(0.1, 0.2, 0.3),
      b = c(1, 2, 3)
    )

    expect_no_error(set_sample_design(X))
    current_X <- set_sample_design()
    expect_s3_class(current_X, "data.frame")
    expect_equal(current_X, X)

    expect_error(
      set_sample_design(1:3),
      "sample_design must be a matrix or data frame"
    )

    reset_sample_desing()
    expect_null(set_sample_design())
  })

  test_that("mcmodule_to_matrices returns correct structure", {
    # Minimal mock mcmodule object
    mcmodule <- list(
      node_list = list(
        a = list(mcnode = array(1:6, dim = c(3, 1, 2))),
        b = list(mcnode = array(7:12, dim = c(3, 1, 2)))
      )
    )
    mats <- mcmodule_to_matrices(mcmodule)
    expect_type(mats, "list")
    expect_equal(length(mats), 2) # 2 variates
    expect_equal(dim(mats[[1]]), c(3, 2))
    expect_equal(dim(mats[[2]]), c(3, 2))
    expect_equal(mats[[1]][1, 1], 1)
    expect_equal(mats[[2]][3, 2], 12)
  })

  test_that("sample_design works with sample_space only", {
    mctable <- data.frame(
      mcnode = c("x", "y"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )
    res <- sample_design(mctable, n = 50)
    expect_s3_class(res, "data.frame")
    expect_type(res$x, "double")
    expect_type(res$y, "double")
    expect_equal(dim(res), c(50, 2))
    expect_equal(colnames(res), c("x", "y"))
    expect_true(all(res$x >= 0 & res$x <= 1))
    expect_true(all(res$y >= 10 & res$y <= 20))
  })

  test_that("sample_design defaults to method='latin' and if_not_sampled='exclude'", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    res <- sample_design(
      mctable,
      n = 20,
      mc_names = c("a", "c")
    )

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "c"))
    expect_equal(nrow(res), 20)
    expect_false(any(grepl("^fix\\.", colnames(res))))
  })

  test_that("sample_design applies transformation and logical coercion by default", {
    mctable <- data.frame(
      mcnode = c("x", "flag", "origin"),
      transformation = c(NA, NA, "ifelse(value == 'always', 1, 0)"),
      sample_space = c(
        "min = 0, max = 1",
        "c(TRUE, FALSE)",
        "c('always', 'sometimes', 'never')"
      ),
      stringsAsFactors = FALSE
    )
    res <- sample_design(mctable, n = 50)

    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(50, 3))
    expect_equal(colnames(res), c("x", "flag", "origin"))
    expect_true(all(res$x >= 0 & res$x <= 1))
    expect_true(all(res$flag %in% c(0, 1)))
    expect_true(all(res$origin %in% c(0, 1)))

    res2 <- sample_design(imports_mctable, n = 50)
    expect_s3_class(res2, "data.frame")
    expect_equal(nrow(res2), 50)
  })

  test_that("sample_design preserves original classes when transformation = FALSE", {
    mctable <- data.frame(
      mcnode = c("x", "flag", "origin"),
      transformation = c(NA, NA, "ifelse(value == 'always', 1, 0)"),
      sample_space = c(
        "min = 0, max = 1",
        "c(TRUE, FALSE)",
        "c('always', 'sometimes', 'never')"
      ),
      stringsAsFactors = FALSE
    )

    res <- sample_design(mctable, n = 50, transformation = FALSE)

    expect_s3_class(res, "data.frame")
    expect_type(res$x, "double")
    expect_type(res$flag, "logical")
    expect_type(res$origin, "character")
    expect_true(all(res$origin %in% c("always", "sometimes", "never")))
  })

  test_that("sample_design with method='morris' generates proper OAT design", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    res <- sample_design(mctable, method = "morris", morris_r = 5)

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "b", "c"))
    expect_true(nrow(res) > 0)
    expect_true(all(res$a >= 0 & res$a <= 1))
    expect_true(all(res$b >= 10 & res$b <= 20))
    expect_true(all(res$c >= -5 & res$c <= 5))
  })

  test_that("sample_design with method='sobol' generates proper LHS design", {
    mctable <- data.frame(
      mcnode = c("x", "y"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )

    set.seed(123)
    res <- sample_design(mctable, n = 50, method = "sobol")

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("x", "y"))
    expect_equal(nrow(res), 50)
    expect_true(all(res$x >= 0 & res$x <= 1))
    expect_true(all(res$y >= 10 & res$y <= 20))
  })

  test_that("sample_design morris method with transformation works", {
    mctable <- data.frame(
      mcnode = c("val", "other"),
      sample_space = c("min = 0, max = 1", "min = -10, max = 10"),
      transformation = c(NA, NA),
      stringsAsFactors = FALSE
    )

    res <- sample_design(
      mctable,
      method = "morris",
      morris_r = 5,
      transformation = TRUE
    )

    expect_s3_class(res, "data.frame")
    expect_type(res$val, "double")
    expect_type(res$other, "double")
  })

  test_that("sample_design sobol method with transformation works", {
    mctable <- data.frame(
      mcnode = c("val", "other"),
      sample_space = c("min = 0, max = 1", "min = -10, max = 10"),
      transformation = c(NA, NA),
      stringsAsFactors = FALSE
    )

    res <- sample_design(
      mctable,
      n = 64,
      method = "sobol",
      transformation = TRUE
    )

    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), 64)
    expect_type(res$val, "double")
    expect_type(res$other, "double")
  })

  test_that("sample_design filters mc_names with latin and if_not_sampled='exclude'", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c", "d"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5",
        "min = 100, max = 200"
      ),
      stringsAsFactors = FALSE
    )

    res <- sample_design(
      mctable,
      n = 20,
      method = "latin",
      mc_names = c("a", "c"),
      if_not_sampled = "exclude"
    )

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "c"))
    expect_equal(nrow(res), 20)
    expect_true(all(res$a >= 0 & res$a <= 1))
    expect_true(all(res$c >= -5 & res$c <= 5))
  })

  test_that("sample_design includes non-sampled with if_not_sampled='median'", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    res <- sample_design(
      mctable,
      n = 15,
      method = "latin",
      mc_names = c("a", "c"),
      if_not_sampled = "median"
    )

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "c", "fix.b"))
    expect_equal(nrow(res), 15)
    expect_length(unique(res$fix.b), 1)
    expect_true(all(res$fix.b >= 10 & res$fix.b <= 20))
  })

  test_that("sample_design morris with mc_names defaults to if_not_sampled='exclude'", {
    mctable <- data.frame(
      mcnode = c("x", "y", "z"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    res <- suppressWarnings(sample_design(
      mctable,
      method = "morris",
      morris_r = 3,
      mc_names = c("x", "z")
    ))

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("x", "z"))
    expect_false(any(grepl("^fix\\.", colnames(res))))
  })

  test_that("sample_design morris with if_not_sampled='median' adds fixed columns", {
    mctable <- data.frame(
      mcnode = c("x", "y", "z"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    res <- suppressWarnings(sample_design(
      mctable,
      method = "morris",
      morris_r = 10,
      mc_names = c("x", "z"),
      if_not_sampled = "median"
    ))

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("x", "z", "fix.y"))
    expect_length(unique(res$fix.y), 1)
    expect_equal(unique(res$fix.y), 15)
  })

  test_that("sample_design sobol with mc_names defaults to if_not_sampled='exclude'", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    res <- sample_design(
      mctable,
      n = 32,
      method = "sobol",
      mc_names = c("a", "c")
    )

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "c"))
    expect_equal(nrow(res), 32)
    expect_false(any(grepl("^fix\\.", colnames(res))))
  })

  test_that("sample_design sobol with if_not_sampled='median' adds fixed columns", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    res <- sample_design(
      mctable,
      n = 32,
      method = "sobol",
      mc_names = c("a", "c"),
      if_not_sampled = "median"
    )

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "c", "fix.b"))
    expect_equal(nrow(res), 32)
    expect_length(unique(res$fix.b), 1)
    expect_equal(unique(res$fix.b), 15)
  })

  test_that("sample_design rejects invalid mc_names", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    expect_error(
      sample_design(mctable, mc_names = c("a", "invalid_node")),
      "Invalid mc_names"
    )
  })
})
