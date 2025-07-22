test_that("ggkegg returns a ggplot object with expected layers", {
  library(tidygraph)
  library(ggraph)
  library(ggplot2)
  library(dplyr)

  # Create a minimal tidygraph object
  nodes <- tibble(
    meta_id = c("n1", "n2"),
    hgnc_symbol = c("TP53", "EGFR"),
    type = c("gene", "gene"),
    x = c(0, 1),
    y = c(0, 1)
  )

  edges <- tibble(
    from = 1,
    to = 2,
    subtype = "activation"
  )

  g <- tbl_graph(nodes = nodes, edges = edges)

  # Plot
  p <- ggkegg(g)

  # Check that it's a ggplot with layers
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 2)

  # Check that some layer has label aesthetics
  has_label_layer <- any(sapply(p$layers, function(layer) {
    "label" %in% names(layer$mapping)
  }))
  expect_true(has_label_layer)

  # Check that manual fill and edge_colour scales are used
  scale_aes <- unlist(lapply(p$scales$scales, function(s) s$aesthetics))
  expect_true("fill" %in% scale_aes)
  expect_true("edge_colour" %in% scale_aes)
})
