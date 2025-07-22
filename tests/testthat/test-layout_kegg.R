test_that("layout_kegg returns valid node layout", {
  kgml <- xml2::read_xml("https://rest.kegg.jp/get/hsa04210/kgml")

  layout_tbl <- layout_kegg(kgml)

  # Basic checks
  expect_s3_class(layout_tbl, "tbl_df")
  expect_true(all(c("id", "x", "y") %in% colnames(layout_tbl)))
  expect_gt(nrow(layout_tbl), 0)

  # All coordinates must be numeric
  expect_type(layout_tbl$x, "double")
  expect_type(layout_tbl$y, "double")

  # No duplicated node ids
  expect_equal(
    sum(duplicated(layout_tbl$id)), 0,
    info = "Duplicated node IDs found in layout"
  )

  # Coordinates must be finite
  expect_true(all(is.finite(layout_tbl$x)))
  expect_true(all(is.finite(layout_tbl$y)))
})

test_that("add_kegg_layout joins layout info to graph nodes by meta_id", {
  library(tidygraph)
  library(tibble)
  library(dplyr)

  # Fake graph with meta_id column
  nodes <- tibble::tibble(name = c("A", "B", "C"), meta_id = c("n1", "n2", "n3"))
  edges <- tibble::tibble(from = 1, to = 2)

  g <- tidygraph::tbl_graph(nodes = nodes, edges = edges)

  # Layout table
  layout_tbl <- tibble::tibble(
    id = c("n1", "n2", "n3"),
    x = c(10, 20, 30),
    y = c(5, 15, 25)
  )

  # Apply layout
  g_layout <- add_kegg_layout(g, layout_tbl)

  # Check result
  layout_result <- as_tibble(g_layout)

  expect_equal(layout_result$x, c(10, 20, 30))
  expect_equal(layout_result$y, c(5, 15, 25))
})

