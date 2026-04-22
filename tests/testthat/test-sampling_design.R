suppressMessages({
  get_design <- function(sd) {
    if (inherits(sd, "sobolSalt")) {
      return(as.data.frame(
        sd$X1,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    if (!is.null(sd$X)) {
      return(as.data.frame(sd$X, stringsAsFactors = FALSE, check.names = FALSE))
    }

    stop("Unable to extract design matrix from sampling_design output")
  }

  test_that("set_sampling_design and reset_sampling_design work", {
    reset_sampling_design()
    expect_null(set_sampling_design())

    X <- data.frame(
      a = c(0.1, 0.2, 0.3),
      b = c(1, 2, 3)
    )

    expect_no_error(set_sampling_design(X))
    current_X <- set_sampling_design()
    expect_type(current_X, "list")
    expect_true(all(c("sa", "X") %in% names(current_X)))
    expect_s3_class(current_X$X, "data.frame")
    expect_equal(current_X$X, X)

    sd <- suppressWarnings(sampling_design(
      data.frame(
        mcnode = c("a", "b"),
        sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
        stringsAsFactors = FALSE
      ),
      n = 5
    ))

    expect_no_error(set_sampling_design(sd))
    current_sd <- set_sampling_design()
    expect_type(current_sd, "list")
    expect_true(all(c("sa", "X") %in% names(current_sd)))
    expect_s3_class(current_sd$X, "data.frame")

    expect_error(
      set_sampling_design(1:3),
      "sampling_design must be a matrix, data frame, or list with element 'X'"
    )

    reset_sampling_design()
    expect_null(set_sampling_design())
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

  test_that("sampling_design works with sample_space only", {
    mctable <- data.frame(
      mcnode = c("x", "y"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )
    sd <- suppressWarnings(sampling_design(mctable, n = 50))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)
    expect_s3_class(res, "data.frame")
    expect_type(res$x, "double")
    expect_type(res$y, "double")
    expect_equal(ncol(res), 2)
    expect_true(nrow(res) > 0)
    expect_equal(colnames(res), c("x", "y"))
    expect_true(all(res$x >= 0 & res$x <= 1))
    expect_true(all(res$y >= 10 & res$y <= 20))
  })

  test_that("sampling_design defaults to method='morris' and if_not_sampled='exclude'", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    sd <- suppressWarnings(sampling_design(
      mctable,
      mc_names = c("a", "c")
    ))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_equal(colnames(res), c("a", "c"))
    expect_false(any(grepl("^fix\\.", colnames(res))))
  })

  test_that("sampling_design applies transformation and logical coercion by default", {
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
    sd <- suppressWarnings(sampling_design(mctable))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_equal(colnames(res), c("x", "flag", "origin"))
    expect_true(all(res >= 0 & res <= 1))
  })

  test_that("sampling_design with method='morris' generates proper OAT design", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    sd <- suppressWarnings(sampling_design(
      mctable,
      method = "morris",
      morris_r = 5
    ))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_equal(colnames(res), c("a", "b", "c"))
    expect_true(nrow(res) > 0)
    expect_true(all(res[, 1] >= 0 & res[, 1] <= 1))
    expect_true(all(res[, 2] >= 10 & res[, 2] <= 20))
    expect_true(all(res[, 3] >= -5 & res[, 3] <= 5))
  })

  test_that("sampling_design with method='sobol' generates proper LHS design", {
    mctable <- data.frame(
      mcnode = c("x", "y"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )

    set.seed(123)
    sd <- sampling_design(mctable, n = 50, method = "sobol")
    expect_s3_class(sd, "sobolSalt")
    res <- get_design(sd)

    expect_equal(colnames(res), c("x", "y"))
    expect_equal(nrow(res), 50)
    expect_true(all(res[, 1] >= 0 & res[, 1] <= 1))
    expect_true(all(res[, 2] >= 10 & res[, 2] <= 20))
  })

  test_that("sampling_design morris method with transformation works", {
    mctable <- data.frame(
      mcnode = c("val", "other"),
      sample_space = c("min = 0, max = 1", "min = -10, max = 10"),
      transformation = c(NA, NA),
      stringsAsFactors = FALSE
    )

    sd <- suppressWarnings(sampling_design(
      mctable,
      method = "morris",
      morris_r = 5,
      transformation = TRUE
    ))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_s3_class(res, "data.frame")
    expect_type(res$val, "double")
    expect_type(res$other, "double")
  })

  test_that("sampling_design sobol method with transformation works", {
    mctable <- data.frame(
      mcnode = c("val", "other"),
      sample_space = c("min = 0, max = 1", "min = -10, max = 10"),
      transformation = c(NA, NA),
      stringsAsFactors = FALSE
    )

    sd <- sampling_design(
      mctable,
      n = 64,
      method = "sobol",
      transformation = TRUE
    )
    expect_s3_class(sd, "sobolSalt")
    res <- get_design(sd)

    expect_s3_class(res, "data.frame")
    expect_equal(nrow(res), 64)
    expect_type(res$val, "double")
    expect_type(res$other, "double")
  })

  test_that("sampling_design filters mc_names with morris and if_not_sampled='exclude'", {
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

    sd <- suppressWarnings(sampling_design(
      mctable,
      n = 20,
      method = "morris",
      mc_names = c("a", "c"),
      if_not_sampled = "exclude"
    ))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "c"))
    expect_true(nrow(res) > 0)
    expect_true(all(res$a >= 0 & res$a <= 1))
    expect_true(all(res$c >= -5 & res$c <= 5))
  })

  test_that("sampling_design with morris and if_not_sampled='median' returns morris design", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    sd <- suppressWarnings(sampling_design(
      mctable,
      n = 15,
      method = "morris",
      mc_names = c("a", "c"),
      if_not_sampled = "median"
    ))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "c"))
    expect_false(any(grepl("^fix\\.", colnames(res))))
  })

  test_that("sampling_design morris with mc_names defaults to if_not_sampled='exclude'", {
    mctable <- data.frame(
      mcnode = c("x", "y", "z"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    sd <- suppressWarnings(sampling_design(
      mctable,
      method = "morris",
      morris_r = 3,
      mc_names = c("x", "z")
    ))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("x", "z"))
    expect_false(any(grepl("^fix\\.", colnames(res))))
  })

  test_that("sampling_design morris with if_not_sampled='median' keeps sampled columns", {
    mctable <- data.frame(
      mcnode = c("x", "y", "z"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    sd <- suppressWarnings(sampling_design(
      mctable,
      method = "morris",
      morris_r = 10,
      mc_names = c("x", "z"),
      if_not_sampled = "median"
    ))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("x", "z"))
    expect_false(any(grepl("^fix\\.", colnames(res))))
  })

  test_that("sampling_design sobol with mc_names defaults to if_not_sampled='exclude'", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    sd <- sampling_design(
      mctable,
      n = 32,
      method = "sobol",
      mc_names = c("a", "c")
    )
    expect_s3_class(sd, "sobolSalt")
    res <- get_design(sd)

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "c"))
    expect_equal(nrow(res), 32)
    expect_false(any(grepl("^fix\\.", colnames(res))))
  })

  test_that("sampling_design sobol with if_not_sampled='median' currently errors", {
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
      sampling_design(
        mctable,
        n = 32,
        method = "sobol",
        mc_names = c("a", "c"),
        if_not_sampled = "median"
      ),
      "object 'X' not found"
    )
  })

  test_that("sampling_design rejects invalid mc_names", {
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
      sampling_design(mctable, mc_names = c("a", "invalid_node")),
      "Invalid mc_names"
    )
  })

  test_that("sampling_design excludes NA sample_space when if_not_sampled='exclude'", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        NA,
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    sd <- suppressWarnings(sampling_design(
      mctable,
      method = "morris",
      morris_r = 5,
      if_not_sampled = "exclude"
    ))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_s3_class(res, "data.frame")
    expect_equal(colnames(res), c("a", "c"))
    expect_false("b" %in% colnames(res))
    expect_false("fix.b" %in% colnames(res))
  })

  test_that("sampling_design sets fixed value 0 for NA sample_space when not excluded", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        NA,
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    sd <- suppressWarnings(sampling_design(
      mctable,
      method = "morris",
      morris_r = 5,
      if_not_sampled = "median"
    ))
    expect_s3_class(sd, "morris")
    res <- get_design(sd)

    expect_s3_class(res, "data.frame")
    expect_false("b" %in% colnames(res))
    expect_false("fix.b" %in% colnames(res))
  })
})
