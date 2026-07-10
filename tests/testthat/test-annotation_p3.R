test_that("annotate_kegg_graph warns on identifiers not in dictionary", {
  dummy_nodes <- tibble::tibble(
    name = "dummy:k00020",
    meta_id = "MID10",
    x = 1, y = 1
  )

  g <-
    igraph::make_empty_graph(n = nrow(dummy_nodes), directed = TRUE) |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(
      name = dummy_nodes$name,
      meta_id = dummy_nodes$meta_id,
      x = dummy_nodes$x,
      y = dummy_nodes$y
    )

  meta_dict <- tibble::tibble(
    kegg_id = "dummy:k00020",
    meta_id = "MID10",
    type = "gene",
    entrez_id = "9999"
  )

  expect_warning(
    annotate_kegg_graph(g, meta_dict, identifiers = c("hgnc_symbol", "entrez_id")),
    regexp = "Some requested identifiers are not present"
  )
})

test_that("annotate_kegg_graph accepts repeated graph node names without warning when annotation is identical", {
  dummy_nodes <- tibble::tibble(
    name = c("dummy:k00030", "dummy:k00030"),
    meta_id = c("MID11", "MID11"),
    x = 1, y = 2
  )

  g <-
    igraph::make_empty_graph(n = nrow(dummy_nodes), directed = TRUE) |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(
      name = dummy_nodes$name,
      meta_id = dummy_nodes$meta_id,
      x = dummy_nodes$x,
      y = dummy_nodes$y
    )
  meta_dict <- tibble::tibble(
    kegg_id = c("dummy:k00030", "dummy:k00030"),
    meta_id = c("MID11", "MID11"),
    type = c("gene", "gene"),
    hgnc_symbol = c("GENE_X", "GENE_X")
  )

  expect_no_warning(
    g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol")
  )

  tib <- tidygraph::as_tibble(g_annot)

  expect_true("hgnc_symbol" %in% names(tib))
  expect_equal(nrow(tib), 2)
  expect_equal(tib$hgnc_symbol, c("GENE_X", "GENE_X"))
})

test_that("annotate_kegg_graph ignores dictionary rows whose KEGG ID is absent from graph", {
  dummy_nodes <- tibble::tibble(
    name = "dummy:k00040",
    meta_id = "MID20",
    x = 1, y = 1
  )

  g <-
    igraph::make_empty_graph(n = nrow(dummy_nodes), directed = TRUE) |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(
      name = dummy_nodes$name,
      meta_id = dummy_nodes$meta_id,
      x = dummy_nodes$x,
      y = dummy_nodes$y
    )

  meta_dict <- tibble::tibble(
    kegg_id = c("dummy:k00040", "dummy:k00041"),
    meta_id = c("MID20", "MID20"),
    type = c("gene", "gene"),
    hgnc_symbol = c("GENE_A", "GENE_B")
  )

  expect_no_warning(
    g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol")
  )

  tib <- tidygraph::as_tibble(g_annot)

  expect_true("hgnc_symbol" %in% names(tib))
  expect_equal(nrow(tib), 1)
  expect_equal(tib$hgnc_symbol[1], "GENE_A")
})

test_that("annotate_kegg_graph warns and collapses conflicting annotations per KEGG ID", {
  dummy_nodes <- tibble::tibble(
    name = c("dummy:k00050", "dummy:k00051"),
    meta_id = c("MID30", "MID31"),
    x = 1, y = 1
  )

  g <-
    igraph::make_empty_graph(n = nrow(dummy_nodes), directed = TRUE) |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(
      name = dummy_nodes$name,
      meta_id = dummy_nodes$meta_id,
      x = dummy_nodes$x,
      y = dummy_nodes$y
    )

  meta_dict <- tibble::tibble(
    kegg_id = c("dummy:k00050", "dummy:k00050", "dummy:k00051", "dummy:k00051"),
    meta_id = c("MID30", "MID30", "MID31", "MID31"),
    type = c("gene", "gene", "gene", "gene"),
    hgnc_symbol = c("GENE_A1", "GENE_A2", "GENE_B1", "GENE_B2")
  )

  expect_warning(
    g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol"),
    regexp = "Multiple annotation values found"
  )

  tib <- tidygraph::as_tibble(g_annot)

  expect_true("hgnc_symbol" %in% names(tib))
  expect_equal(nrow(tib), 2)
  expect_equal(tib$hgnc_symbol, c("GENE_A1", "GENE_B1"))
})

test_that("annotate_kegg_graph does not keep dictionary matches absent from graph", {
  dummy_nodes <- tibble::tibble(
    name = "dummy:k00060",
    meta_id = "MID40",
    x = 1, y = 1
  )

  g <-
    igraph::make_empty_graph(n = nrow(dummy_nodes), directed = TRUE) |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(
      name = dummy_nodes$name,
      meta_id = dummy_nodes$meta_id,
      x = dummy_nodes$x,
      y = dummy_nodes$y
    )

  meta_dict <- tibble::tibble(
    kegg_id = c("dummy:k00060", "dummy:k00060b"),
    meta_id = c("MID40", "MID40"),
    type = c("gene", "gene"),
    hgnc_symbol = c("GENE_X1", "GENE_X2")
  )

  expect_no_warning(
    g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol")
  )

  tib <- tidygraph::as_tibble(g_annot)

  expect_true("hgnc_symbol" %in% names(tib))
  expect_equal(nrow(tib), 1)
  expect_equal(tib$hgnc_symbol[1], "GENE_X1")
})
