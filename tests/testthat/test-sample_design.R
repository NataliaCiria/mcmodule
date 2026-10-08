suppressMessages({
  test_that("set_sample_design and reset_sample_design manage the global design", {
    # Start clean
    reset_sample_design()
    expect_null(set_sample_design())

    X <- data.frame(a = c(0.1, 0.2), b = c(1, 2), stringsAsFactors = FALSE)
    expect_message(set_sample_design(X), "sample_design set")

    current <- set_sample_design()
    expect_type(current, "list")
    expect_true(all(c("sa", "X") %in% names(current)))
    expect_null(current$sa)
    expect_true(is.data.frame(current$X))
    expect_equal(current$X, X)

    expect_message(reset_sample_design(), "sample_design reset")
    expect_null(set_sample_design())
    reset_sample_design()
  })

  test_that("set_sample_design accepts a list input with X", {
    X <- data.frame(a = c(1, 2), stringsAsFactors = FALSE)
    obj <- list(sa = "dummy", X = X)

    set_sample_design(obj)
    current <- set_sample_design()
    expect_equal(current$sa, "dummy")
    expect_equal(current$X, X)
    reset_sample_design()
  })

  test_that("apply_value_transformation returns value unchanged for missing or empty transformations", {
    expect_equal(apply_value_transformation(c(1, 2), NA), c(1, 2))
    expect_equal(apply_value_transformation(c(1, 2), ""), c(1, 2))
    expect_equal(apply_value_transformation(c(1, 2), "   "), c(1, 2))
    expect_equal(apply_value_transformation(c(1, 2), NULL), c(1, 2))
  })

  test_that("apply_value_transformation evaluates expressions and coerces logicals to numeric", {
    expect_equal(apply_value_transformation(c(1, 2), "value * 10"), c(10, 20))
    expect_equal(apply_value_transformation(c(1, 2), "  value + 1  "), c(2, 3))
    expect_identical(apply_value_transformation(c(0, 2), "value > 1"), c(0, 1))
  })

  test_that("mctable_bounds errors when required columns are missing", {
    mctable <- data.frame(
      mcnode = "x",
      stringsAsFactors = FALSE
    )
    expect_error(
      mctable_bounds(mctable),
      "mctable must contain columns 'mcnode' and 'sample_space'"
    )
  })

  test_that("mctable_bounds supports categorical sample_space via numeric transformation", {
    set.seed(456)
    mctable <- data.frame(
      mcnode = "x",
      sample_space = "c('always','sometimes','never')",
      transformation = "ifelse(value == 'always', 1, ifelse(value == 'sometimes', 0.5, 0))",
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, transformation = TRUE, n_probe = 2000)

    expect_equal(b$factors, "x")
    # Expected bounds after mapping {never, sometimes, always} -> {0, 0.5, 1}.
    expect_equal(b$binf[[1]], 0)
    expect_equal(b$bsup[[1]], 1)
  })

  test_that("mctable_bounds uses n_probe to approximate bounds for non-monotone transformations", {
    set.seed(123)
    mctable <- data.frame(
      mcnode = c("x"),
      sample_space = c("min = 0, max = 1"),
      # Non-monotone on [0, 1], maximum at value = 0.3
      transformation = c("-(value - 0.3)^2"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, transformation = TRUE, n_probe = 5000)
    expect_equal(b$factors, "x")
    # True bounds are [-0.49, 0]. Probing should get close to 0 for bsup.
    expect_true(b$binf[[1]] <= -0.45)
    expect_true(b$bsup[[1]] > -0.01)
    expect_true(b$bsup[[1]] <= 0.001)
  })

  test_that("mctable_bounds returns numeric bounds and factor names", {
    mctable <- data.frame(
      mcnode = c("x", "y"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable)
    expect_type(b, "list")
    expect_true(all(c("binf", "bsup", "factors", "fixed") %in% names(b)))
    expect_equal(b$factors, c("x", "y"))
    expect_type(b$binf, "double")
    expect_type(b$bsup, "double")
    expect_equal(b$binf, c(0, 10))
    expect_equal(b$bsup, c(1, 20))
    expect_equal(length(b$fixed), 0)
  })

  test_that("mctable_bounds supports c(min, max) bounds", {
    mctable <- data.frame(
      mcnode = c("x", "y"),
      sample_space = c("c(0, 1)", "c(10, 20)"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable)
    expect_equal(b$binf, c(0, 10))
    expect_equal(b$bsup, c(1, 20))
  })

  test_that("mctable_bounds filters mc_names and errors on invalid names", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, mc_names = c("a", "c"))
    expect_equal(b$factors, c("a", "c"))
    expect_equal(b$binf, c(0, -5))
    expect_equal(b$bsup, c(1, 5))

    expect_error(
      mctable_bounds(mctable, mc_names = c("a", "nope")),
      "Invalid mc_names"
    )
  })

  test_that("mctable_bounds drops NA/empty sample_space from factors by default", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c", "d"),
      sample_space = c("min = 0, max = 1", NA, "", "NA"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable)
    expect_equal(b$factors, "a")
    expect_equal(b$binf, 0)
    expect_equal(b$bsup, 1)
    expect_equal(length(b$fixed), 0)
  })

  test_that("mctable_bounds sets fixed values for non-sampled nodes", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c", "d"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        NA,
        "NA"
      ),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(
      mctable,
      mc_names = "a",
      if_not_sampled = "median",
      transformation = FALSE
    )

    expect_equal(b$factors, "a")
    expect_equal(b$binf, 0)
    expect_equal(b$bsup, 1)
    expect_true(all(c("b", "c", "d") %in% names(b$fixed)))
    expect_equal(unname(b$fixed[["b"]]), 15)
    expect_equal(unname(b$fixed[["c"]]), 0)
    expect_equal(unname(b$fixed[["d"]]), 0)
  })

  test_that("mctable_bounds applies transformation to bounds when requested", {
    set.seed(1)
    mctable <- data.frame(
      mcnode = c("x"),
      sample_space = c("min = 0, max = 1"),
      transformation = c("value^2"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, transformation = TRUE, n_probe = 2000)
    expect_equal(b$factors, "x")
    expect_true(b$binf[[1]] >= 0)
    expect_true(b$bsup[[1]] <= 1)
  })

  test_that("mctable_bounds errors for unsupported bounds formats", {
    mctable <- data.frame(
      mcnode = c("cat"),
      sample_space = c("c('a','b')"),
      stringsAsFactors = FALSE
    )

    expect_error(
      mctable_bounds(mctable),
      "Cannot extract numeric bounds"
    )
  })


  test_that("mctable_bounds keeps constant factors when drop_constant = FALSE", {
    mctable <- data.frame(
      mcnode = c("x", "k"),
      sample_space = c("min = 0, max = 1", "min = 5, max = 5"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, drop_constant = FALSE)
    expect_true("dropped" %in% names(b))
    expect_equal(b$factors, c("x", "k"))
    expect_equal(b$binf, c(0, 5))
    expect_equal(b$bsup, c(1, 5))
    expect_equal(b$dropped, character(0))
  })

  test_that("mctable_bounds drops constant factors by default (drop_constant = TRUE)", {
    mctable <- data.frame(
      mcnode = c("x", "k", "j"),
      sample_space = c("min = 0, max = 1", "min = 5, max = 5", "c(2, 2)"),
      stringsAsFactors = FALSE
    )

    # drop_constant = TRUE is the default
    expect_message(
      b <- mctable_bounds(mctable),
      "Dropped 2 input\\(s\\) with no variation"
    )
    expect_equal(b$factors, "x")
    expect_equal(b$binf, 0)
    expect_equal(b$bsup, 1)
    expect_equal(b$dropped, c("k", "j"))
    # if_not_sampled = "exclude" (default): constants are not added to fixed
    expect_equal(length(b$fixed), 0)
  })

  test_that("mctable_bounds adds dropped constants to fixed when if_not_sampled != 'exclude'", {
    mctable <- data.frame(
      mcnode = c("x", "k", "y"),
      sample_space = c("min = 0, max = 1", "min = 5, max = 5", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(
      mctable,
      mc_names = c("x", "k"),
      if_not_sampled = "median",
      transformation = FALSE,
      drop_constant = TRUE
    )

    expect_equal(b$factors, "x")
    expect_equal(b$dropped, "k")
    expect_true(all(c("y", "k") %in% names(b$fixed)))
    expect_equal(unname(b$fixed[["y"]]), 15)
    expect_equal(unname(b$fixed[["k"]]), 5)
  })

  test_that("mctable_bounds drops factors made constant by a transformation", {
    set.seed(10)
    mctable <- data.frame(
      mcnode = c("x", "flag"),
      sample_space = c("min = 0, max = 1", "min = 1, max = 5"),
      # value > 0 is always TRUE on [1, 5], so the transformed range collapses to 1
      transformation = c(NA, "value > 0"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(
      mctable,
      if_not_sampled = "median",
      transformation = TRUE,
      drop_constant = TRUE,
      n_probe = 500
    )

    expect_equal(b$factors, "x")
    expect_equal(b$dropped, "flag")
    expect_equal(unname(b$fixed[["flag"]]), 1)
  })

  test_that("mctable_bounds warns when drop_constant removes all factors", {
    mctable <- data.frame(
      mcnode = "k",
      sample_space = "min = 5, max = 5",
      stringsAsFactors = FALSE
    )

    expect_warning(
      b <- mctable_bounds(mctable, drop_constant = TRUE),
      "no factors remain"
    )
    expect_equal(b$factors, character(0))
    expect_equal(b$binf, numeric(0))
    expect_equal(b$bsup, numeric(0))
    expect_equal(b$dropped, "k")
  })

  test_that("mctable_bounds keeps precision of transformed bounds (no false constants)", {
    set.seed(42)
    mctable <- data.frame(
      mcnode = c("x", "big"),
      sample_space = c("min = 0.1000001, max = 0.1000002", "min = 1000000, max = 1000001"),
      transformation = c("value", "value"),
      stringsAsFactors = FALSE
    )

    # With sprintf("%g") both bounds would round to the same value and be dropped
    b <- mctable_bounds(
      mctable,
      transformation = TRUE,
      drop_constant = TRUE,
      n_probe = 200
    )

    expect_equal(b$factors, c("x", "big"))
    expect_equal(b$dropped, character(0))
    expect_true(all(b$bsup > b$binf))
  })

  test_that("mctable_bounds applies transformation to fixed values of non-sampled nodes", {
    mctable <- data.frame(
      mcnode = c("x", "y"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      transformation = c(NA, "value / 10"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(
      mctable,
      mc_names = "x",
      if_not_sampled = "median",
      transformation = TRUE
    )

    expect_equal(unname(b$fixed[["y"]]), 1.5)
  })

  test_that("mctable_sobol_matrices returns mapped draws for runif bounds", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = c("a", "b"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )

    X <- mctable_sobol_matrices(
      mctable = mctable,
      N = 32,
      order = "first"
    )

    expect_true(is.matrix(X))
    expect_equal(ncol(X), 2)
    expect_true(all(X[, 1] >= 0 & X[, 1] <= 1))
    expect_true(all(X[, 2] >= 10 & X[, 2] <= 20))
  })

  test_that("mctable_sobol_matrices maps rnorm using qnorm", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = "x",
      mc_func = "rnorm",
      sample_space = "mean = 0, sd = 1",
      stringsAsFactors = FALSE
    )

    X <- mctable_sobol_matrices(
      mctable = mctable,
      N = 64,
      order = "first"
    )

    expect_true(is.matrix(X))
    expect_equal(ncol(X), 1)
    expect_true(all(is.finite(X[, 1])))
    expect_true(abs(mean(X[, 1])) < 0.25)
    expect_true(sd(X[, 1]) > 0.5)
  })

  test_that("mctable_sobol_matrices supports mc_names", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20", NA),
      stringsAsFactors = FALSE
    )

    X <- mctable_sobol_matrices(
      mctable = mctable,
      N = 16,
      order = "first",
      mc_names = "a"
    )

    expect_true(is.matrix(X))
    expect_equal(ncol(X), 1)
  })

  test_that("eval_module fills missing sample_design inputs from mctable sample_space", {
    # Only 'a' is provided in sample_design; 'b' is required by expression.
    sample_design <- data.frame(a = c(0, 1), stringsAsFactors = FALSE)

    mctable <- data.frame(
      mcnode = c("a", "b"),
      mc_func = NA,
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )

    expr <- quote({
      out <- a + b
    })

    m <- eval_module(
      exp = expr,
      data = NULL,
      mctable = mctable,
      sample_design = sample_design,
      if_not_sampled = "median"
    )

    expect_true(inherits(m, "mcmodule"))
    expect_true(isTRUE(m$node_list$b$from_sample_design))
    expect_true(isTRUE(m$node_list$b$from_sample_design_fixed))
    # b fixed at mean(10, 20) = 15 for both samples
    expect_equal(as.numeric(m$node_list$b$mcnode[, 1, 1]), c(15, 15))
    # out = a + b
    expect_equal(as.numeric(m$node_list$out$mcnode[, 1, 1]), c(15, 16))
  })

  test_that("eval_module if_not_sampled supports min/max", {
    sample_design <- data.frame(a = c(0, 1), stringsAsFactors = FALSE)
    mctable <- data.frame(
      mcnode = c("a", "b"),
      mc_func = NA,
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )
    expr <- quote({
      out <- a + b
    })

    m_min <- eval_module(
      exp = expr,
      data = NULL,
      mctable = mctable,
      sample_design = sample_design,
      if_not_sampled = "min"
    )
    expect_equal(as.numeric(m_min$node_list$b$mcnode[, 1, 1]), c(10, 10))

    m_max <- eval_module(
      exp = expr,
      data = NULL,
      mctable = mctable,
      sample_design = sample_design,
      if_not_sampled = "max"
    )
    expect_equal(as.numeric(m_max$node_list$b$mcnode[, 1, 1]), c(20, 20))
  })

  test_that("eval_module errors only when missing from sample_design and cannot be created from mctable", {
    sample_design <- data.frame(a = c(0, 1), stringsAsFactors = FALSE)
    mctable <- data.frame(
      mcnode = c("a"),
      mc_func = NA,
      sample_space = c("min = 0, max = 1"),
      stringsAsFactors = FALSE
    )

    expr <- quote({
      out <- a + b
    })

    expect_error(
      eval_module(
        exp = expr,
        data = NULL,
        mctable = mctable,
        sample_design = sample_design
      ),
      "Input 'b' is missing from sample_design and not found in mctable"
    )
  })

  # ---- normalise_mc_func() / qsample_space() ----

  test_that("normalise_mc_func strips namespaces and handles missing values", {
    expect_equal(normalise_mc_func("rpert"), "rpert")
    expect_equal(normalise_mc_func("mc2d::rpert"), "rpert")
    expect_equal(normalise_mc_func("mc2d:::rpert"), "rpert")
    expect_equal(normalise_mc_func(" stats::rnorm "), "rnorm")
    expect_identical(normalise_mc_func(NA), NA_character_)
    expect_identical(normalise_mc_func(""), NA_character_)
    expect_identical(normalise_mc_func(NULL), NA_character_)
  })

  test_that("qsample_space maps probabilities through the sample_space distribution", {
    u <- c(0, 0.5, 1)

    # Uniform from bounds (both formats), exact endpoints
    expect_equal(qsample_space(u, "min = 10, max = 20"), c(10, 15, 20))
    expect_equal(qsample_space(u, "c(10, 20)"), c(10, 15, 20))

    # rnorm: median equals mean, extremes are finite
    x_norm <- qsample_space(u, "mean = 50, sd = 5", mc_func = "rnorm")
    expect_equal(x_norm[[2]], 50)
    expect_true(all(is.finite(x_norm)))
    expect_true(x_norm[[1]] < 20 && x_norm[[3]] > 80)

    # rpert (also namespace-qualified)
    ss_pert <- "min = 0, mode = 0.2, max = 1"
    expect_equal(
      qsample_space(0.5, ss_pert, mc_func = "rpert"),
      mc2d::qpert(0.5, min = 0, mode = 0.2, max = 1)
    )
    expect_equal(
      qsample_space(u, ss_pert, mc_func = "mc2d::rpert"),
      qsample_space(u, ss_pert, mc_func = "rpert")
    )

    # Single value and categorical
    expect_equal(qsample_space(u, "value = 5"), c(5, 5, 5))
    expect_equal(
      qsample_space(c(0, 0.3, 0.5, 0.9, 1), "c('a', 'b', 'c')"),
      c("a", "a", "b", "c", "c")
    )
  })

  test_that("qsample_space falls back to uniform when distribution parameters are incomplete", {
    # rnorm without mean/sd but with bounds (as in imports_mctable$animals_n)
    expect_equal(
      qsample_space(c(0, 1), "min = 82, max = 176", mc_func = "rnorm"),
      c(82, 176)
    )
    # rpert without mode
    expect_equal(
      qsample_space(c(0, 1), "min = 0, max = 1", mc_func = "rpert"),
      c(0, 1)
    )
  })

  test_that("qsample_space errors for unsupported or incomplete definitions", {
    expect_error(
      qsample_space(0.5, "min = 0, max = 1", mc_func = "rbeta", node_name = "x"),
      "Unsupported mc_func 'rbeta' for 'x'"
    )
    expect_error(
      qsample_space(0.5, "c('a', 'b')", mc_func = "runif", node_name = "x"),
      "runif requires min/max"
    )
    expect_error(
      qsample_space(0.5, "mean = 0", mc_func = "rnorm", node_name = "x"),
      "must provide mean and sd for rnorm"
    )
    # Named numeric values without bounds or mc_func are not treated as categories
    expect_error(
      qsample_space(0.5, "mean = 0, sd = 1", node_name = "x"),
      "Cannot map sample_space for 'x'"
    )
  })

  # ---- Probing (sample_from_space) ----

  test_that("sample_from_space is deterministic and does not consume the random stream", {
    set.seed(1)
    seed_before <- .Random.seed

    p1 <- sample_from_space("min = 0, max = 1", 11)
    p2 <- sample_from_space("min = 0, max = 1", 11)

    expect_identical(.Random.seed, seed_before)
    expect_identical(p1, p2)
    expect_equal(p1, seq(0, 1, by = 0.1))
  })

  test_that("mctable_bounds uses mc_func when probing transformations (rnorm)", {
    mctable <- data.frame(
      mcnode = "x",
      mc_func = "rnorm",
      sample_space = "mean = 50, sd = 5",
      transformation = "value / 100",
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, transformation = TRUE)

    # Previously probed only {5, 50} (sd and mean as categories): [0.05, 0.5]
    expect_false(isTRUE(all.equal(c(b$binf, b$bsup), c(0.05, 0.5))))
    expect_true(b$binf[[1]] < 0.3)
    expect_true(b$bsup[[1]] > 0.7)
  })

  test_that("mctable_bounds probing returns exact endpoints for monotone transformations", {
    mctable <- data.frame(
      mcnode = "x",
      sample_space = "min = 0, max = 1",
      transformation = "value * 2",
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, transformation = TRUE)
    expect_equal(b$binf, 0)
    expect_equal(b$bsup, 2)
  })

  test_that("mctable_bounds does not drop inputs that vary only near the bounds", {
    mctable <- data.frame(
      mcnode = "x",
      sample_space = "min = 0, max = 1",
      transformation = "value > 0.999",
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, transformation = TRUE, drop_constant = TRUE)
    expect_equal(b$factors, "x")
    expect_equal(b$dropped, character(0))
    expect_equal(b$binf, 0)
    expect_equal(b$bsup, 1)
  })

  test_that("mctable_bounds does not consume the random stream", {
    mctable <- data.frame(
      mcnode = "x",
      sample_space = "min = 0, max = 1",
      transformation = "value^2",
      stringsAsFactors = FALSE
    )

    set.seed(2)
    seed_before <- .Random.seed
    mctable_bounds(mctable, transformation = TRUE)
    expect_identical(.Random.seed, seed_before)
  })

  test_that("transformations returning only NA are skipped (sample_space on model scale)", {
    # As in imports_mctable$test_origin: categorical mapping, numeric sample_space
    mctable <- data.frame(
      mcnode = "test_origin",
      sample_space = "min = 0, max = 1",
      transformation = "ifelse(value == 'always', 1, ifelse(value == 'never', 0, NA))",
      stringsAsFactors = FALSE
    )

    expect_message(
      b <- mctable_bounds(mctable, transformation = TRUE),
      "assuming sample_space is already on the model scale"
    )
    expect_equal(b$binf, 0)
    expect_equal(b$bsup, 1)

    expect_equal(
      transform_sample_values(c(0, 0.5), mctable$transformation, "test_origin"),
      c(0, 0.5)
    )
    # Valid transformations are applied as usual
    expect_equal(transform_sample_values(c(1, 2), "value * 10"), c(10, 20))
  })

  # ---- fixed_value_for_node() ----

  test_that("fixed_value_for_node computes fixed values from bounds and transformation", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c("min = 10, max = 20", "min = 10, max = 20", NA),
      transformation = c(NA, "value / 10", NA),
      stringsAsFactors = FALSE
    )

    expect_equal(fixed_value_for_node(mctable, "a"), 15)
    expect_equal(fixed_value_for_node(mctable, "a", if_not_sampled = "max"), 20)
    expect_equal(fixed_value_for_node(mctable, "a", if_not_sampled = "min"), 10)
    expect_equal(fixed_value_for_node(mctable, "b"), 1.5)
    expect_equal(fixed_value_for_node(mctable, "b", transformation = FALSE), 15)

    expect_error(
      fixed_value_for_node(mctable, "c"),
      "Input 'c' is missing from sample_design and has no numeric bounds"
    )
    expect_error(
      fixed_value_for_node(mctable, "nope"),
      "Input 'nope' is missing from sample_design and not found in mctable"
    )
  })

  # ---- mctable_sobol_matrices(): transformation, drop_constant, mc_func ----

  test_that("mctable_sobol_matrices applies transformations by default", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = c("a", "b"),
      sample_space = c("min = 0, max = 1", "min = 0, max = 1"),
      transformation = c("value * 10", NA),
      stringsAsFactors = FALSE
    )

    X <- mctable_sobol_matrices(mctable, N = 32)
    X_raw <- mctable_sobol_matrices(mctable, N = 32, transformation = FALSE)

    expect_equal(as.numeric(X[, 1]), as.numeric(X_raw[, 1]) * 10)
    expect_equal(as.numeric(X[, 2]), as.numeric(X_raw[, 2]))
    expect_true(all(X_raw[, 1] >= 0 & X_raw[, 1] <= 1))
  })

  test_that("mctable_sobol_matrices supports categorical sample_space with a transformation", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = "x",
      sample_space = "c('always', 'sometimes', 'never')",
      transformation = "ifelse(value == 'always', 1, ifelse(value == 'sometimes', 0.5, 0))",
      stringsAsFactors = FALSE
    )

    X <- mctable_sobol_matrices(mctable, N = 64)
    expect_true(all(X[, 1] %in% c(0, 0.5, 1)))
    expect_equal(sort(unique(as.numeric(X[, 1]))), c(0, 0.5, 1))

    expect_error(
      mctable_sobol_matrices(mctable, N = 64, transformation = FALSE),
      "categorical sample_space; provide a numeric transformation"
    )
  })

  test_that("mctable_sobol_matrices drops constant inputs by default (drop_constant = TRUE)", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = c("a", "k", "flag", "b"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 5, max = 5",
        "min = 1, max = 5",
        "min = 10, max = 20"
      ),
      transformation = c(NA, NA, "value > 0", NA),
      stringsAsFactors = FALSE
    )

    X_all <- mctable_sobol_matrices(mctable, N = 16, drop_constant = FALSE)
    expect_equal(attr(X_all, "dropped"), character(0))
    expect_equal(ncol(X_all), 4)

    # drop_constant = TRUE is the default
    expect_message(
      X <- mctable_sobol_matrices(mctable, N = 16),
      "Dropped 2 input\\(s\\) with no variation: k, flag"
    )
    expect_equal(colnames(X), c("a", "b"))
    expect_equal(attr(X, "dropped"), c("k", "flag"))
    # A, B and one AB block per remaining factor
    expect_equal(nrow(X), 16 * (2 + 2))
  })

  test_that("mctable_sobol_matrices errors when drop_constant removes all inputs", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = "k",
      sample_space = "min = 5, max = 5",
      stringsAsFactors = FALSE
    )

    expect_error(
      suppressMessages(
        mctable_sobol_matrices(mctable, N = 16, drop_constant = TRUE)
      ),
      "No sampled factors"
    )
  })

  test_that("mctable_sobol_matrices accepts namespace-qualified mc_func and rejects unsupported ones", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = "x",
      mc_func = "mc2d::rpert",
      sample_space = "min = 0.8, mode = 0.875, max = 0.91",
      stringsAsFactors = FALSE
    )

    X <- mctable_sobol_matrices(mctable, N = 32)
    expect_true(all(X[, 1] >= 0.8 & X[, 1] <= 0.91))

    mctable$mc_func <- "rbeta"
    expect_error(
      mctable_sobol_matrices(mctable, N = 32),
      "Unsupported mc_func 'rbeta'"
    )
  })

  test_that("mctable_sobol_matrices works with imports_mctable (transformations skipped where not applicable)", {
    skip_if_not_installed("sensobol")

    X <- suppressMessages(mctable_sobol_matrices(imports_mctable, N = 16))
    expect_true(all(is.finite(X)))
    expect_true("test_origin" %in% colnames(X))
    expect_true(all(X[, "test_origin"] >= 0 & X[, "test_origin"] <= 1))
  })

  # ---- eval_module(): fixed values on the transformed scale ----

  test_that("eval_module applies mctable transformation to fixed values of non-sampled inputs", {
    sample_design <- data.frame(a = c(0, 1), stringsAsFactors = FALSE)
    mctable <- data.frame(
      mcnode = c("a", "b"),
      mc_func = NA,
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      transformation = c(NA, "value / 10"),
      stringsAsFactors = FALSE
    )

    m <- eval_module(
      exp = list(fixed_exp = quote({
        out <- a + b
      })),
      data = NULL,
      mctable = mctable,
      sample_design = sample_design,
      if_not_sampled = "median"
    )

    expect_true(isTRUE(m$node_list$b$from_sample_design_fixed))
    # b fixed at mean(10, 20) / 10 = 1.5
    expect_equal(as.numeric(m$node_list$b$mcnode[, 1, 1]), c(1.5, 1.5))
    expect_equal(as.numeric(m$node_list$out$mcnode[, 1, 1]), c(1.5, 2.5))
  })

  test_that("constant inputs dropped by mctable_sobol_matrices() are created as fixed nodes by eval_module()", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = c("a", "k"),
      mc_func = NA,
      sample_space = c("min = 0, max = 1", "min = 5, max = 5"),
      stringsAsFactors = FALSE
    )

    X <- suppressMessages(mctable_sobol_matrices(mctable, N = 8))
    expect_equal(colnames(X), "a")
    expect_equal(attr(X, "dropped"), "k")

    m <- eval_module(
      exp = list(sobol_exp = quote({
        out <- a + k
      })),
      data = NULL,
      mctable = mctable,
      sample_design = X
    )

    expect_true(isTRUE(m$node_list$k$from_sample_design_fixed))
    expect_equal(as.numeric(m$node_list$k$mcnode), rep(5, nrow(X)))
    expect_equal(
      as.numeric(m$node_list$out$mcnode),
      as.numeric(X[, "a"]) + 5
    )
  })

  test_that("parse_sample_space_bounds treats a single numeric value as min = max", {
    for (ss in c("1", "c(1)", "c('1')", 'c("1")', "value = 1")) {
      expect_equal(parse_sample_space_bounds(ss), c(min = 1, max = 1), info = ss)
    }
    expect_equal(parse_sample_space_bounds("0.25"), c(min = 0.25, max = 0.25))
    expect_equal(parse_sample_space_bounds("-1e-3"), c(min = -1e-3, max = -1e-3))

    # Non-numeric single values stay categorical (no bounds)
    expect_null(parse_sample_space_bounds("c('always')"))
    expect_null(parse_sample_space_bounds("value = always"))

    # Single distribution parameters are not constants (only "value = X")
    expect_null(parse_sample_space_bounds("mean = 0"))

    # Existing formats are unchanged
    expect_equal(parse_sample_space_bounds("c(0, 1)"), c(min = 0, max = 1))
    expect_equal(
      parse_sample_space_bounds("min = 0, max = 1"),
      c(min = 0, max = 1)
    )
    expect_error(parse_sample_space("always"), "Unsupported sample_space format")
  })

  test_that("mctable_bounds handles single-value sample_space as constant inputs", {
    mctable <- data.frame(
      mcnode = c("a", "k1", "k2", "k3", "k4"),
      mc_func = c(NA, NA, "runif", NA, NA),
      sample_space = c("min = 0, max = 1", "1", "c(1)", "c('1')", "value = 1"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, drop_constant = FALSE)
    expect_equal(b$factors, mctable$mcnode)
    expect_equal(b$binf, c(0, 1, 1, 1, 1))
    expect_equal(b$bsup, c(1, 1, 1, 1, 1))

    expect_message(
      b2 <- mctable_bounds(mctable, if_not_sampled = "median"),
      "Dropped 4 input\\(s\\) with no variation"
    )
    expect_equal(b2$factors, "a")
    expect_equal(b2$dropped, c("k1", "k2", "k3", "k4"))
    expect_equal(unname(b2$fixed[c("k1", "k2", "k3", "k4")]), rep(1, 4))
  })

  test_that("mctable_sobol_matrices drops single-value inputs and eval_module recreates them", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = c("a", "k1", "k2", "k3", "k4"),
      mc_func = c(NA, NA, "runif", NA, NA),
      sample_space = c("min = 0, max = 1", "1", "c(1)", "c('1')", "value = 1"),
      stringsAsFactors = FALSE
    )

    X <- suppressMessages(mctable_sobol_matrices(mctable, N = 8))
    expect_equal(colnames(X), "a")
    expect_equal(attr(X, "dropped"), c("k1", "k2", "k3", "k4"))

    m <- eval_module(
      exp = list(single_exp = quote({
        out <- a + k1 + k2 + k3 + k4
      })),
      data = NULL,
      mctable = mctable,
      sample_design = X
    )

    for (k in c("k1", "k2", "k3", "k4")) {
      expect_true(isTRUE(m$node_list[[k]]$from_sample_design_fixed), info = k)
      expect_equal(as.numeric(m$node_list[[k]]$mcnode), rep(1, nrow(X)), info = k)
    }
    expect_equal(as.numeric(m$node_list$out$mcnode), as.numeric(X[, "a"]) + 4)
  })

  test_that("check_mctable accepts a single numeric sample_space value", {
    mctable <- data.frame(
      mcnode = c("k1", "k2"),
      mc_func = NA,
      sample_space = c("1", " 0.5 "),
      stringsAsFactors = FALSE
    )
    expect_no_error(suppressWarnings(check_mctable(mctable)))

    mctable$sample_space[2] <- "always"
    expect_error(
      suppressWarnings(check_mctable(mctable)),
      "Invalid sample_space format at row\\(s\\): 2"
    )
  })
})
