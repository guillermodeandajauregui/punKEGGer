test_that("annotate_kegg_graph handles multiple identifier columns with NAs", {
  dummy_nodes <- tibble::tibble(
    name = "dummy:k00010",
    meta_id = "MID",
    x = 1, y = 1
  )
  g <- tidygraph::as_tbl_graph(dummy_nodes)

  meta_dict <- tibble::tibble(
    kegg_id = "dummy:k00010",
    meta_id = "MID",
    type = "gene",
    hgnc_symbol = NA,
    entrez_id = "1234"
  )

  g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = c("hgnc_symbol", "entrez_id"))
  tib <- tidygraph::as_tibble(g_annot)

  expect_true("hgnc_symbol" %in% names(tib))
  expect_true("entrez_id" %in% names(tib))
  expect_true(is.na(tib$hgnc_symbol[1]))
  expect_equal(tib$entrez_id[1], "1234")
})

test_that("annotate_kegg_graph ignores dictionary entries not present in graph", {
  dummy_nodes <- tibble::tibble(
    name = "dummy:k00011",
    meta_id = "MID2",
    x = 1, y = 1
  )
  g <- tidygraph::as_tbl_graph(dummy_nodes)

  meta_dict <- tibble::tibble(
    kegg_id = "dummy:k99999",  # not in graph
    meta_id = "MID3",
    type = "gene",
    hgnc_symbol = "GENE_X"
  )

  g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol")
  tib <- tidygraph::as_tibble(g_annot)

  expect_true("hgnc_symbol" %in% names(tib))
  expect_true(is.na(tib$hgnc_symbol[1]))
})

test_that("annotate_kegg_graph handles multiple entries per meta_id", {
  dummy_nodes <- tibble::tibble(
    name = "dummy:k00012",
    meta_id = "MID4",
    x = 1, y = 1
  )
  g <- tidygraph::as_tbl_graph(dummy_nodes)

  meta_dict <- tibble::tibble(
    kegg_id = c("dummy:k00012", "dummy:k00012b"),
    meta_id = c("MID4", "MID4"),
    type = c("gene", "gene"),
    hgnc_symbol = c("GENE_A", "GENE_B")
  )

  g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol")

  tib <- tidygraph::as_tibble(g_annot)
  expect_true("hgnc_symbol" %in% names(tib))
  expect_true(!is.na(tib$hgnc_symbol[1]))
})

test_that("create_meta_dict handles multiple kegg_ids for one meta_id", {
  node_info <- tibble::tibble(
    id = c("n1", "n2", "n3"),
    kegg_id = c("k1", "k2", "k3"),
    meta_id = c("M1", "M1", "M2"),
    type = c("gene", "gene", "gene")
  )

  kegg_dict <- tibble::tibble(
    kegg_id = c("k1", "k2", "k3"),
    hgnc_symbol = c("A", "B", "C")
  )

  meta_dict <- create_meta_dict(node_info, kegg_dict)

  expect_equal(
    meta_dict |> dplyr::filter(meta_id == "M1") |> dplyr::pull(hgnc_symbol),
    c("A", "B")
  )

  expect_equal(
    meta_dict |> dplyr::filter(meta_id == "M2") |> dplyr::pull(hgnc_symbol),
    "C"
  )
})
