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


test_that("mcmodule_composition handles combined modules", {
  module_a <- list(
    exp = list(
      exp_a = quote({
        a <- 1
      })
    ),
    node_list = list(a = list(type = "out_node")),
    data = list()
  )
  class(module_a) <- "mcmodule"

  result <- mcmodule_composition(module_a)

  expect_equal(result$is_combined, FALSE)
  expect_equal(result$n_modules, 1)
  expect_equal(result$module_names, "module_a")
  expect_s3_class(result$module_exp, "data.frame")
  expect_equal(nrow(result$module_exp), 1)
  expect_equal(result$module_exp$module, "module_a")
  expect_equal(result$module_exp$exp, "exp_a")

  module_b <- list(
    exp = list(
      exp_b1 = quote({
        b1 <- 2
      }),
      exp_b2 = quote({
        b2 <- b1 + 3
      })
    ),
    node_list = list(b = list(type = "out_node")),
    data = list()
  )
  class(module_b) <- "mcmodule"

  combined_ab <- combine_modules(module_a, module_b)

  result <- mcmodule_composition(combined_ab)

  expect_equal(result$n_modules, 2)
  expect_equal(result$is_combined, TRUE)
  expect_equal(result$module_names, c("module_a", "module_b"))
  expect_s3_class(result$module_exp, "data.frame")
  expect_equal(nrow(result$module_exp), 3) # 1 from module_a + 2 from module_b
  expect_equal(result$module_exp$module, c("module_a", "module_b", "module_b"))
  expect_equal(result$module_exp$exp, c("exp_a", "exp_b1", "exp_b2"))

  module_c <- list(
    exp = list(
      exp_c = quote({
        c <- a + b2
      })
    ),
    node_list = list(c = list(type = "out_node")),
    data = list()
  )
  class(module_c) <- "mcmodule"

  combined_abc <- combine_modules(combined_ab, module_c)
  result <- mcmodule_composition(combined_abc)

  expect_equal(result$n_modules, 3)
  expect_equal(result$is_combined, TRUE)
  expect_equal(result$module_names, c("module_a", "module_b", "module_c"))
  expect_s3_class(result$module_exp, "data.frame")
  expect_equal(nrow(result$module_exp), 4) # 1 + 2 + 1
  expect_equal(
    result$module_exp$module,
    c("module_a", "module_b", "module_b", "module_c")
  )
  expect_equal(result$module_exp$exp, c("exp_a", "exp_b1", "exp_b2", "exp_c"))
})
