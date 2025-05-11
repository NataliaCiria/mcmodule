#' @import dplyr

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

test_that("visNetwork functions generate correct formats", {
  nodes <- visNetwork_nodes(imports_mcmodule)
  edges <- visNetwork_edges(imports_mcmodule)

  expect_true(all(c("id", "color", "module", "expression", "title") %in% colnames(nodes)))
  expect_true(all(c("from", "to", "id") %in% colnames(edges)))

  imports_network<-mc_network(imports_mcmodule)
  expect_true(all(c("visNetwork", "htmlwidget") %in% class(imports_network)))
  expect_equal(imports_network$x$nodes[names(nodes)], nodes)
  expect_equal(imports_network$x$edges[names(edges)], edges)
})
