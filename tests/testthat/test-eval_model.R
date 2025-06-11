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

    previous_module<-trial_totals(previous_module,
                                  mc_names="no_detect_a",
                                  trials_n = "animals_n",
                                  subsets_n = "farms_n",
                                  subsets_p = "h_prev",
                                  mctable = imports_mctable)

    #  Create current_module
    current_data  <-data.frame(pathogen=c("a","a","a","b","b","b","b"),
                               origin = c("east","south","nord","east","south","nord","nord"),
                              scenario_id=c("0","0","0","0","0","0","clean_transport"),
                              survival_p_min=c(0.7,0.7,0.7,0.7,0.7,0.7,0.1),
                              survival_p_max=c(0.8,0.8,0.8,0.8,0.8,0.8,0.15))

    current_data_keys <-list(current_data = list(data=current_data, keys=c("pathogen","scenario_id")))

    current_mctable  <- data.frame(mcnode = c("survival_p"),
                                   description = c("Survival probability"),
                                   mc_func = c("runif"),
                                   from_variable = c(NA),
                                   transformation = c(NA),
                                   sensi_analysis = c(FALSE))
    current_exp<-quote({
      imported_contaminated <- no_detect_a_set * survival_p
    })

    # TODO
    expect_error(
      current_module <- eval_model(
        model_exp = c(current = current_exp),
        data = current_data,
        mctable = current_mctable,
        data_keys = current_data_keys,
        prev_mcmodule = previous_module
      ),
      "use wif_match()"
    )
    a<-wif_match(current_data,imports_data)

    current_module <- eval_model(
      model_exp = c(current = current_exp),
      data = a$current_data,
      mctable = current_mctable,
      data_keys = current_data_keys,
      prev_mcmodule = previous_module
    )

    combined_module<-combine_modules(previous_module,current_module)

    combined_module<-at_least_one(combined_module, c("no_detect_a","imported_contaminated"), name="total")

    mc_network(combined_module)
    mc_summary(combined_module, "survival_p")
    mc_summary(combined_module, "no_detect_a_set")
    mc_summary(combined_module, "no_detect_a")

    # CHECK mc_match_data NULL when should be 0
    mc_summary(combined_module, "imported_contaminated")

    combined_module$node_list$total

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


