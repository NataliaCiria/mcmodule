test_that("combine_modules works", {
  # Create test mcmodules
  module_x <- list(
    data = list(data_x = data.frame(x = 1:3)),
    node_list = list(
      node1 = list(type = "in_node"),
      node2 = list(type = "out_node")
    ),
    exp = quote({
      node2 <- node1 * 2
    })
  )

  module_y <- list(
    data = list(data_y = data.frame(y = 4:6)),
    node_list = list(node3 = list(type = "out_node")),
    exp = quote({
      node3 <- node1 + node2
    })
  )

  # Test combination
  result <- combine_modules(module_x, module_y)

  expect_type(result, "list")
  expect_equal(names(result$data), c("data_x", "data_y"))
  expect_equal(names(result$node_list), c("node1", "node2", "node3"))
})

test_that("mcmodule_info handles combined modules", {
  data_ab = data.frame(key1 = c("x", "y"))
  module_a <- list(
    exp = list(
      exp_a = quote({
        a <- 1
      })
    ),
    node_list = list(
      a = list(
        type = "out_node",
        exp_name = "exp_a",
        data_name = "data_ab",
        keys = c("key1")
      )
    ),
    data = list(data_ab = data_ab)
  )
  class(module_a) <- "mcmodule"

  result <- mcmodule_info(module_a)

  expect_equal(result$is_combined, FALSE)
  expect_equal(result$n_modules, 1)
  expect_equal(result$module_names, "module_a")
  expect_s3_class(result$module_exp_data, "data.frame")
  expect_equal(nrow(result$module_exp_data), 1)
  expect_equal(result$module_exp_data$module, "module_a")
  expect_equal(result$module_exp_data$exp, "exp_a")
  expect_equal(result$global_keys, "key1")

  module_b <- list(
    exp = list(
      exp_b1 = quote({
        b1 <- 2
      }),
      exp_b2 = quote({
        b2 <- b1 + 3
      })
    ),
    node_list = list(
      b1 = list(
        type = "out_node",
        exp_name = "exp_b1",
        data_name = "data_ab",
        keys = c("key1")
      ),
      b2 = list(
        type = "out_node",
        exp_name = "exp_b2",
        data_name = "data_ab",
        keys = c("key1")
      )
    ),
    data = list(data_ab = data_ab)
  )
  class(module_b) <- "mcmodule"

  combined_ab <- combine_modules(module_a, module_b)

  result <- mcmodule_info(combined_ab)

  expect_equal(result$n_modules, 2)
  expect_equal(result$is_combined, TRUE)
  expect_equal(result$module_names, c("module_a", "module_b"))
  expect_s3_class(result$module_exp_data, "data.frame")
  expect_equal(nrow(result$module_exp_data), 3) # 1 from module_a + 2 from module_b
  expect_equal(
    result$module_exp_data$module,
    c("module_a", "module_b", "module_b")
  )
  expect_equal(result$module_exp_data$exp, c("exp_a", "exp_b1", "exp_b2"))

  data_c = data.frame(
    key1 = c("x", "x", "y", "y"),
    key2 = c("a", "b", "a", "b")
  )

  module_c <- list(
    exp = list(
      exp_c = quote({
        c <- a + b2
      })
    ),
    node_list = list(
      c = list(
        type = "out_node",
        exp_name = "exp_c",
        data_name = "data_c",
        keys = c("key1", "key2")
      )
    ),
    data = list(data_c = data_c)
  )
  class(module_c) <- "mcmodule"

  combined_abc <- combine_modules(combined_ab, module_c)
  result <- mcmodule_info(combined_abc)

  expect_equal(result$n_modules, 3)
  expect_equal(result$is_combined, TRUE)
  expect_equal(result$module_names, c("module_a", "module_b", "module_c"))
  expect_s3_class(result$module_exp_data, "data.frame")
  expect_equal(nrow(result$module_exp_data), 4) # 1 + 2 + 1
  expect_equal(
    result$module_exp_data$module,
    c("module_a", "module_b", "module_b", "module_c")
  )
  expect_equal(
    result$module_exp_data$exp,
    c("exp_a", "exp_b1", "exp_b2", "exp_c")
  )

  # node counts and traceability info should be present
  det <- mcmodule_info(combined_abc)
  expect_true(is.list(det))
  expect_true("module_exp_data" %in% names(det))
  # If node_counts available, it should be a data.frame with module names
  if ("node_counts" %in% names(det)) {
    expect_s3_class(det$node_counts, "data.frame")
    expect_true(all(det$node_counts$module %in% det$module_names))
  }
})


test_that("combine_modules retains one copy of identical shared nodes", {
  shared_node <- list(
    type = "out_node",
    module = "origin",
    exp_name = "origin_exp",
    data_name = "shared_data",
    mcnode = c(0.1, 0.2)
  )
  module_first <- list(
    data = list(shared_data = data.frame(id = 1:2)),
    exp = list(first = quote({ first <- shared_node })),
    node_list = list(
      shared_node = shared_node,
      first = list(type = "out_node", module = "first")
    )
  )
  module_second <- list(
    data = module_first$data,
    exp = list(second = quote({ second <- shared_node })),
    node_list = list(
      shared_node = shared_node,
      second = list(type = "out_node", module = "second")
    )
  )

  result <- combine_modules(module_first, module_second)

  expect_identical(names(result$node_list), c("shared_node", "first", "second"))
  expect_identical(result$node_list$shared_node, shared_node)
  expect_identical(result$data, module_first$data)
  expect_identical(result$exp, list(
    module_first = module_first$exp,
    module_second = module_second$exp
  ))
  expect_identical(anyDuplicated(names(result$node_list)), 0L)
})

test_that("identical nodes are compared before assigning module metadata", {
  shared_node <- list(type = "out_node", mcnode = 1)
  module_x <- list(
    data = list(), exp = list(), node_list = list(shared = shared_node)
  )
  module_y <- list(
    data = list(), exp = list(), node_list = list(shared = shared_node)
  )

  result <- combine_modules(module_x, module_y)

  expect_identical(names(result$node_list), "shared")
  expect_identical(result$node_list$shared$module, "module_x")
  expect_null(module_x$node_list$shared$module)
  expect_null(module_y$node_list$shared$module)
})

test_that("combine_modules rejects non-identical same-named nodes", {
  module_x <- list(
    data = list(), exp = list(),
    node_list = list(shared = list(type = "out_node", mcnode = c(1, 2)))
  )
  module_y <- module_x
  module_y$node_list$shared$mcnode <- c(1, 3)

  expect_error(
    combine_modules(module_x, module_y),
    "Modules contain conflicting node names: shared"
  )

  # Different metadata is also a conflict, even when values match.
  module_y <- module_x
  module_y$node_list$shared$data_name <- "other_data"
  expect_error(
    combine_modules(module_x, module_y),
    "Modules contain conflicting node names: shared"
  )
})

test_that("combine_modules checks repeated names within an input module", {
  shared_node <- list(type = "out_node", module = "origin", mcnode = 1)
  module_x <- list(
    data = list(), exp = list(),
    node_list = list(shared = shared_node, shared = shared_node)
  )
  module_y <- list(data = list(), exp = list(), node_list = list())

  result <- combine_modules(module_x, module_y)
  expect_identical(names(result$node_list), "shared")
  expect_identical(result$node_list$shared, shared_node)

  module_x$node_list[[2]]$mcnode <- 2
  expect_error(
    combine_modules(module_x, module_y),
    "Modules contain conflicting node names: shared"
  )
})

test_that("combined identical nodes can be used in a later eval_module", {
  previous_data <- data.frame(id = 1:2, base_value = c(1, 2))
  previous_module <- suppressMessages(eval_module(
    exp = list(previous = quote({ shared_node <- base_value * 2 })),
    data = previous_data,
    keys = "id",
    mctable = data.frame(mcnode = "base_value", mc_func = NA),
    data_keys = NULL,
    sample_design = NULL
  ))
  copied_module <- previous_module
  combined <- combine_modules(previous_module, copied_module)

  expect_identical(anyDuplicated(names(combined$node_list)), 0L)
  expect_equal(sum(names(combined$node_list) == "shared_node"), 1L)

  result <- suppressMessages(eval_module(
    exp = list(current = quote({ result <- shared_node * 2 })),
    prev_mcmodule = combined,
    data = previous_data,
    keys = "id",
    mctable = NULL,
    data_keys = NULL,
    sample_design = NULL
  ))

  expect_equal(as.numeric(result$node_list$result$mcnode), c(4, 8))
})
