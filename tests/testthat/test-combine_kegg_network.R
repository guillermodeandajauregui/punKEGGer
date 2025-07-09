test_that("combine_kegg_network produces graph with correct meta_id", {
  kgml <- xml2::read_xml("https://rest.kegg.jp/get/hsa04210/kgml")
  g <- combine_kegg_network(kgml)

  # Check class
  expect_s3_class(g, "tbl_graph")

  # Check node columns
  node_df <- tidygraph::as_tibble(g, active = "nodes")
  expect_true(all(c("name", "meta_id") %in% colnames(node_df)))

  # meta_id must be non-NA and character
  expect_false(any(is.na(node_df$meta_id)))
  expect_type(node_df$meta_id, "character")

  # If a node is in a group, meta_id should differ from name
  diff_count <- sum(node_df$meta_id != node_df$name)
  expect_gt(diff_count, 0)  # At least some nodes should be in groups

  # Check graph structure: at least 1 edge and 5 nodes
  expect_gt(igraph::gsize(g), 1)
  expect_gt(igraph::gorder(g), 5)
})
