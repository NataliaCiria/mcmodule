test_that("get_edge_table works", {
  # Create test mcmodule
  edges <- get_edge_table(imports_mcmodule)
  expect_true(all(names(imports_mcmodule$node_list)%in%edges$node_to))
  expect_true(all(c("node_from", "node_to") %in% colnames(edges)))
})

test_that("get_node_table works", {
  nodes <- get_node_table(imports_mcmodule)
  expect_true(all(names(imports_mcmodule$node_list)%in%nodes$name))
})

test_that("mc_network works", {
  nodes <- visNetwork_nodes(imports_mcmodule)
  edges <- visNetwork_edges(imports_mcmodule)

  expect_true(all(c("id", "color", "module", "expression", "title") %in% colnames(nodes)))
  expect_true(all(c("from", "to", "id") %in% colnames(edges)))

  imports_network<-mc_network(imports_mcmodule)
  expect_true(all(c("visNetwork", "htmlwidget") %in% class(imports_network)))
  expect_equal(imports_network$x$nodes[names(nodes)], nodes)
  expect_equal(imports_network$x$edges[names(edges)], edges)
})

test_that("combined nodes mc_network works", {
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
  combined_module<-combine_modules(previous_module,current_module)

  combined_module<-at_least_one(combined_module, c("no_detect_a","imported_contaminated"), name="total")

  mc_network(combined_module)

})
