suppressMessages({
  test_that("eval_model works", {
    # Test basic functionality
    result <- eval_model(
      model_exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    # Check class and structure
    expect_equal(class(result), "mcmodule")
    expect_true(all(c("data", "model_exp", "node_list", "modules") %in% names(result)))

    # Test error handling for missing prev_mcmodule
    test_exp_prev <- quote({
      result <- prev_value * 2
    })
    expect_error(
      eval_model(
        model_exp = test_exp_prev,
        data = imports_data,
        mctable = imports_mctable,
        data_keys = imports_data_keys
      ),
      "prev_mcmodule.*needed but not provided"
    )

    # Test with multiple expressions
    model_exp_list <- list(
      imports = imports_exp,
      additional = quote({
        final_result <- no_detect_a * 2
      })
    )

    multi_result <- eval_model(
      model_exp = model_exp_list,
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    # Verify multiple modules were created
    expect_equal(length(multi_result$modules), 2)
    expect_true(all(c("imports", "additional") %in% multi_result$modules))

    # Check that variables from first module are available in second
    expect_true("no_detect_a" %in% names(multi_result$node_list))
    expect_true("final_result" %in% names(multi_result$node_list))
  })

  test_that("eval_model gets previous nodes", {
    #  Create previous_module
    previous_module <- eval_model(
      model_exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    #  Create current_module
    current_data  <-data.frame(pathogen=c("a","b","a","b"),
                              origin=c("nord","nord","nord","nord"),
                              scenario_id=c("0","0","no_product_imports","no_product_imports"),
                              contaminated=c(0.1,0.5,0.1,0.5),
                              imported=c(1,1,0,0),
                              products_n=c(1500,1500,0,0))

    current_data_keys <-list(current_data = list(data=current_data, keys=c("pathogen","origin","scenario_id")))

    current_mctable  <- data.frame(mcnode = c("contaminated", "imported", "products_n"),
                                   description = c("Probability a product is contaminated", "Probability a product is imported", "Number of products"),
                                   mc_func = c(NA, NA, NA),
                                   from_variable = c(NA, NA, NA),
                                   transformation = c(NA, NA, NA),
                                   sensi_analysis = c(FALSE, FALSE, FALSE))
    current_exp<-quote({
      imported_contaminated <- contaminated * imported
    })

    current_module <- eval_model(
      model_exp = c(current = current_exp),
      data = current_data,
      mctable = current_mctable,
      data_keys = current_data_keys
    )



  })

  test_that("get_mcmodule_nodes works", {
    # Create test data
    test_node <- list(mcnode = "test_value")
    test_node_list <- list(
      node1 = list(mcnode = "value1"),
      node2 = list(mcnode = "value2")
    )
    class(test_node_list) <- "mcnode_list"

    test_mcmodule <- list(node_list = test_node_list)
    class(test_mcmodule) <- "mcmodule"

    # Test with mcmodule object
    result1 <- get_mcmodule_nodes(test_mcmodule, c("node1"))
    expect_equal(length(result1), 1)
    expect_equal(result1$node1$mcnode, "value1")

    # Test with mcnode_list object
    result2 <- get_mcmodule_nodes(test_node_list, c("node2"))
    expect_equal(length(result2), 1)
    expect_equal(result2$node2$mcnode, "value2")

    # Test with invalid input
    expect_error(get_mcmodule_nodes("invalid"))

    # Test with non-existent nodes
    result3 <- get_mcmodule_nodes(test_mcmodule, c("non_existent"))
    expect_equal(length(result3), 0)

    # Test with no nodes specified
    result4 <- get_mcmodule_nodes(test_mcmodule)
    expect_equal(length(result4), 0)
  })

})


