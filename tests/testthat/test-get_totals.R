suppressMessages({
  test_that("at_least_one works", {
    # Create test module
    test_module <- list(
      node_list = list(
        p1 = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(0.1, 0.2, 0.3), type="0", nvariates = 3),
                          max=mcdata(c(0.2, 0.3, 0.4), type="0", nvariates = 3),
                          nvariates = 3),
          data_name = "test_data",
          keys=c("category")
        ),
        p2 = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(0.5, 0.6, 0.7), type="0", nvariates = 3),
                          max=mcdata(c(0.6, 0.7, 0.8), type="0", nvariates = 3),
                          nvariates = 3),
          data_name = "test_data",
          keys=c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C"),
          scenario_id = c("0", "0", "0")
        )
      )
    )

    # Test basic functionality
    result <- at_least_one(test_module, c("p1", "p2"), name="p_combined")

    # Check node attributes
    expect_equal(result$node_list$p_combined$type, "total")
    expect_equal(result$node_list$p_combined$param, c("p1", "p2"))

    # Test error on missing nodes
    expect_error(at_least_one(test_module, c("p1", "missing")),
                 "missing not found in mcmodule")
  })

  test_that("at_least_one match works", {
    # Create test module
    test_module <- list(
      node_list = list(
        p1 = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(0.1, 0.2, 0.3), type="0", nvariates = 3),
                          max=mcdata(c(0.2, 0.3, 0.4), type="0", nvariates = 3),
                          nvariates = 3),
          data_name = "data_x",
          keys=c("category")
        ),
        p2 = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(0.5, 0.6, 0.7), type="0", nvariates = 3),
                          max=mcdata(c(0.6, 0.7, 0.8), type="0", nvariates = 3),
                          nvariates = 3),
          data_name = "data_y",
          keys=c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B", "C"),
          scenario_id = c("0", "0", "0")
        ),
        data_y = data.frame(
          category = c("B", "B", "B"),
          scenario_id = c("0", "1", "2")
        )
      )
    )

    # Test basic functionality
    result <- at_least_one(test_module, c("p1", "p2"), name="p_combined")

    # Check node attributes
    expect_equal(result$node_list$p_combined$type, "total")
    expect_equal(result$node_list$p_combined$param, c("p1", "p2"))

    # Verify expected keys_xy
    expect_equal(result$node_list$p_combined$summary$category, c("A","B","C","B","B"))

    # Test error on missing nodes
    expect_error(at_least_one(test_module, c("p1", "missing")),
                 "missing not found in mcmodule")
  })

  test_that("generate_all_name works", {
    # Basic functionality
    expect_equal(generate_all_name(c("test_a", "test_b")), "test_all")
    expect_equal(generate_all_name(c("good_special_a", "good_special_b", "good_special_top")),
                 "good_special_all")

    # Error cases
    expect_error(generate_all_name(c("good_special_a", "bad_special_b")),
                 "Input strings do not share a common prefix")
    expect_error(generate_all_name(c("test_a", "test_all")),
                 "One of the mc_names already contains '_all' suffix")
  })

  test_that("agg_totals works correctly", {
    # Create test data
    test_module <- list(
      node_list = list(
        p1 = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(0.1, 0.2, 0.3), type="0", nvariates = 3),
                          max=mcdata(c(0.2, 0.3, 0.4), type="0", nvariates = 3),
                          nvariates = 3),
          data_name = "test_data",
          keys=c("category")
        ),
        p2 = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(0.5, 0.6, 0.7), type="0", nvariates = 3),
                          max=mcdata(c(0.6, 0.7, 0.8), type="0", nvariates = 3),
                          nvariates = 3),
          data_name = "test_data",
          keys=c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C"),
          scenario_id = c("0", "0", "0")
        )
      )
    )

    # Test cases
    result <- agg_totals(test_module, "p1")
    expect_type(result, "list")
    expect_equal(result$node_list[["p1_agg"]]$description,
                 "Combined probability assuming independence by: scenario_id")

    # Test aggregation methods
    result_sum <- agg_totals(test_module, "p1", agg_func="sum")
    expect_equal(result_sum$node_list[["p1_agg"]]$description,
                 "Sum by: scenario_id")


    result_avg <- agg_totals(test_module, "p1", agg_func="avg")
    expect_equal(result_avg$node_list[["p1_agg"]]$description,
                 "Average value by: scenario_id")

    # Test error handling
    expect_error(agg_totals(test_module, "test_node", agg_func="invalid"))
    expect_error(agg_totals(test_module, "nonexistent_node"))
  })

  test_that("trial_totals works correctly", {
    # Create test data
    test_module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(0.1, 0.2, 0.3,0.5), type="0", nvariates = 4),
                          max=mcdata(c(0.2, 0.3, 0.4,0.6), type="0", nvariates = 4),
                          nvariates = 4),
          data_name = "test_data",
          keys=c("category","scenario_id")
        ),
        p_2 = list(
          mcnode = mcstoc(runif,
                          min=mcdata(c(0.5, 0.6, 0.7,0.8), type="0", nvariates = 4),
                          max=mcdata(c(0.6, 0.7, 0.8,0.9), type="0", nvariates = 4),
                          nvariates = 4),
          data_name = "test_data",
          keys=c("category","scenario_id")
        ),
        times_n = list(
          mcnode = mcdata(c(3,4,5,6), type="0", nvariates = 4),
          data_name = "test_data",
          keys=c("category","scenario_id")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A","B"),
          scenario_id = c("0", "0", "1", "1"),
          times_n = c(3,4,5,6),
          sites_n_min = c(2,2,2,2),
          sites_n_max = c(2,3,4,5)
        )
      )
    )

    test_mctable<-data.frame(
      mcnode=c("sites_n"),
      description=c("Number of sites"),
      mc_func= c("runif"),
      from_variable=c(NA),
      transformation=c(NA),
      sensi_analysis=c(FALSE)
    )
    set_mctable(test_mctable)

    result<-trial_totals(test_module, mc_names=c("p_1","p_2"),trials_n = "times_n")

    result<-trial_totals(test_module,
                         mc_names=c("p_1","p_2"),
                         trials_n = "times_n",
                         subsets_p = "sites_n")
    names(result$node_list)
    result<-trial_totals(test_module,
                         mc_names = "p_1",
                         trials_n = "times_n",
                         subsets_n = "sites_n",
                         subsets_p = "p_2")
    names(result$node_list)

    result<-trial_totals(test_module,
                         mc_names = "p_1",
                         trials_n = "times_n",
                         subsets_n = "sites_n",
                         subsets_p = "p_2",
                         agg_keys = "scenario_id")
    names(result$node_list)

    result<-trial_totals(test_module,
                         mc_names = "p_1",
                         trials_n = "times_n",
                         subsets_n = "sites_n",
                         subsets_p = "p_2",
                         agg_keys = "scenario_id",
                         keep_variates = TRUE)

    names(result$node_list)

    reset_mctable()

  })


})

