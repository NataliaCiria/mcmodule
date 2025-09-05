suppressMessages({
  test_that("eval_module works", {
    # Test basic functionality
    result <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    # Check class and structure
    expect_equal(class(result), "mcmodule")
    expect_true(all(c("data", "exp", "node_list", "modules") %in% names(result)))

    # Test error handling for missing prev_mcmodule
    test_exp_prev <- quote({
      result <- prev_value * 2
    })
    expect_error(
      eval_module(
        exp = test_exp_prev,
        data = imports_data,
        mctable = imports_mctable,
        data_keys = imports_data_keys
      ),
      "prev_mcmodule.*needed but not provided"
    )

    # Test with multiple expressions
    exp_list <- list(
      imports = imports_exp,
      additional = quote({
        final_result <- no_detect_a * 2
      })
    )

    multi_result <- eval_module(
      exp = exp_list,
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    # Verify multiple modules were created
    expect_equal(multi_result$modules, c("imports", "additional"))

    # Check that variables from first module are available in second
    expect_true("no_detect_a" %in% names(multi_result$node_list))
    expect_true("final_result" %in% names(multi_result$node_list))

    # Check that inputs have the right metadata
    expect_equal(combined_module$node_list$test_sensi$keys,c("pathogen"))
    expect_equal(combined_module$node_list$test_sensi$input_dataset,c("test_sensitivity"))

    expect_equal(combined_module$node_list$w_prev$keys,c("pathogen", "origin"))
    expect_equal(combined_module$node_list$w_prev$input_dataset,c("prevalence_region"))

    # Check that outputs have the right metadata
    expect_equal(combined_module$node_list$no_detect_a$keys,c("pathogen", "origin"))
    expect_equal(combined_module$node_list$no_detect_a$inputs,c("false_neg_a", "no_test_a"))

    expect_equal(combined_module$node_list$no_detect_a_set$keys,c("pathogen", "origin"))
    expect_equal(combined_module$node_list$no_detect_a_set$inputs,c("no_detect_a", "animals_n", "farms_n", "h_prev"))

  })

  test_that("eval_module gets previous nodes", {
    #  Create previous_module
    previous_module <- eval_module(
      exp = c(imports = imports_exp),
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
                               clean = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
                              scenario_id=c("0","0","0","0","0","0","clean_transport"),
                              survival_p_min=c(0.7,0.7,0.7,0.7,0.7,0.7,0.1),
                              survival_p_max=c(0.8,0.8,0.8,0.8,0.8,0.8,0.15))

    current_data_keys <-list(survival = list(cols=c("pathogen", "clean","survival_p_min", "survival_p_max"),
                                             keys=c("pathogen", "clean")))

    current_mctable  <- data.frame(mcnode = c("survival_p"),
                                   description = c("Survival probability"),
                                   mc_func = c("runif"),
                                   from_variable = c(NA),
                                   transformation = c(NA),
                                   sensi_analysis = c(FALSE))
    current_exp<-quote({
      imported_contaminated <- no_detect_a_set * survival_p
    })

    current_module <- eval_module(
      exp = c(current = current_exp),
      data = current_data,
      mctable = current_mctable,
      data_keys = current_data_keys,
      prev_mcmodule = previous_module)

    combined_module<-combine_modules(previous_module,current_module)

    combined_module<-at_least_one(combined_module, c("no_detect_a","imported_contaminated"), name="total")

    expect_equal(combined_module$node_list$no_detect_a$keys,c("pathogen","origin"))
    summary1<-mc_summary(combined_module, "no_detect_a_set")
    expect_equal(summary1$pathogen,c("a","a","a","b","b","b"))

    expect_equal(combined_module$node_list$survival_p$keys,c("pathogen","clean"))
    expect_equal(combined_module$node_list$survival_p$input_dataset,c("survival"))
    summary2<-mc_summary(combined_module, "survival_p")
    expect_equal(summary2$pathogen,c("a","a","a","b","b","b","b"))

    expect_equal(combined_module$node_list$imported_contaminated$keys,c("pathogen","origin","clean")) # union
    summary3<-mc_summary(combined_module, "imported_contaminated")
    expect_equal(summary3$scenario_id,c(0,0,0,0,0,0,"clean_transport"))

    expect_equal(combined_module$node_list$total$keys,c("scenario_id","pathogen","origin")) # intersection
    expect_message(mc_summary(combined_module, "total"), "Too many data names. Using existing summary." )

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


