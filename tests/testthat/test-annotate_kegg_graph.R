test_that("annotate_kegg_graph adds valid identifiers with a clean dictionary", {
  kgml <- xml2::read_xml("https://rest.kegg.jp/get/hsa04210/kgml")

  g_raw <- combine_kegg_network(kgml)
  node_info <- extract_kegg_nodes(kgml)
  g_exp <- expand_metagraph(g_raw, node_info, node_types = c("gene"))

  dict_path <- testthat::test_path("test_dict.csv")
  kegg_dict <- readr::read_csv(dict_path, show_col_types = FALSE)

  duplicates <- kegg_dict |> dplyr::count(kegg_id) |> dplyr::filter(n > 1)
  expect_equal(
    nrow(duplicates), 0,
    info = "Your dictionary has duplicated KEGG IDs. Please clean it before running this test."
  )

  meta_dict <- create_meta_dict(node_info, kegg_dict)

  expect_warning(
    g_annotated <- annotate_kegg_graph(
      g_exp, meta_dict,
      identifiers = c("hgnc_symbol", "entrez_id")
    ),
    regexp = "Multiple meta_id values found"
  )

  expect_s3_class(g_annotated, "tbl_graph")

  cols <- c("meta_id", "hgnc_symbol", "entrez_id")
  for (col in cols) {
    expect_true(
      col %in% colnames(tidygraph::as_tibble(g_annotated)),
      info = paste("Expected column", col, "not found in annotated graph.")
    )
  }
})

test_that("annotate_kegg_graph leaves NA when meta_id not found in dictionary", {
  dummy_nodes <- tibble::tibble(
    name = "dummy:k00001",
    meta_id = "UNMATCHED_ID",
    x = 1, y = 1
  )
  g <- tidygraph::as_tbl_graph(dummy_nodes)

  meta_dict <- tibble::tibble(
    kegg_id = "dummy:k99999",
    meta_id = "MATCHED_ID",
    type = "gene",
    hgnc_symbol = "GENE1"
  )

  g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol")

  expect_true("hgnc_symbol" %in% colnames(tidygraph::as_tibble(g_annot)))
  expect_true(is.na(tidygraph::as_tibble(g_annot)$hgnc_symbol[1]))
})

test_that("annotate_kegg_graph handles duplicated meta_ids in graph", {
  dummy_nodes <- tibble::tibble(
    name = c("dummy:k00002", "dummy:k00002-copy"),
    meta_id = c("DUP_ID", "DUP_ID"),
    x = 1:2, y = 1:2
  )
  g <- tidygraph::as_tbl_graph(dummy_nodes)

  meta_dict <- tibble::tibble(
    kegg_id = c("dummy:k00002", "dummy:k00002-copy"),
    meta_id = c("DUP_ID", "DUP_ID"),
    type = "gene",
    hgnc_symbol = c("GENE_DUP", "GENE_DUP")
  )

  g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol")

  tib <- tidygraph::as_tibble(g_annot) |> dplyr::filter(meta_id == "DUP_ID")
  expect_equal(tib$hgnc_symbol, c("GENE_DUP", "GENE_DUP"))
})

test_that("annotate_kegg_graph throws error if identifier column missing", {
  dummy_nodes <- tibble::tibble(
    name = "dummy:k00003",
    meta_id = "TEST_ID",
    x = 1, y = 1
  )
  g <- tidygraph::as_tbl_graph(dummy_nodes)

  meta_dict <- tibble::tibble(
    kegg_id = "dummy:k00003",
    meta_id = "TEST_ID",
    type = "gene"
  )

  expect_error(
    annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol"),
    regexp = "None of the requested identifiers were found"
  )
})
