test_that("annotate_kegg_graph adds valid identifiers with a clean dictionary", {
  kgml <- xml2::read_xml("https://rest.kegg.jp/get/hsa04210/kgml")

  g_raw <- combine_kegg_network(kgml)
  node_info <- extract_kegg_nodes(kgml)
  g_exp <- expand_metagraph(g_raw, node_info, node_types = c("gene"))

  dict_path <- testthat::test_path("test_dict.csv")
  kegg_dict <- readr::read_csv(dict_path, show_col_types = FALSE)

  # 🔒 Assert that the dictionary has no duplicated KEGG IDs
  duplicates <- kegg_dict |> dplyr::count(kegg_id) |> dplyr::filter(n > 1)
  expect_equal(
    nrow(duplicates), 0,
    info = "Your dictionary has duplicated KEGG IDs. Please clean it before running this test."
  )

  meta_dict <- create_meta_dict(node_info, kegg_dict)

  g_annotated <- annotate_kegg_graph(
    g_exp, meta_dict,
    identifiers = c("hgnc_symbol", "entrez_id")
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
