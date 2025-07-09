test_that("expand_metagraph returns graph with expanded KEGG IDs", {
  kgml <- xml2::read_xml("https://rest.kegg.jp/get/hsa04210/kgml")

  g_raw <- combine_kegg_network(kgml)
  node_info <- extract_kegg_nodes(kgml)

  g_exp <- expand_metagraph(g_raw, node_info, node_types = c("gene", "compound"))

  # Graph is tidygraph
  expect_s3_class(g_exp, "tbl_graph")

  # All node names should look like hsa:#### or similar
  node_names <- igraph::V(g_exp)$name
  expect_true(all(stringr::str_detect(node_names, "^hsa:\\d+$")))

  # No NA node names
  expect_false(any(is.na(node_names)))

  # No edges with NA
  edge_df <- tidygraph::as_tibble(g_exp, active = "edges")
  expect_false(any(is.na(edge_df$from)))
  expect_false(any(is.na(edge_df$to)))
})
