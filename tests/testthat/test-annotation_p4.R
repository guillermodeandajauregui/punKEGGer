test_that("annotate_kegg_graph triggers multi_meta warning when same KEGG ID maps to multiple meta_id", {
  g <- igraph::make_ring(5) |>
    tidygraph::as_tbl_graph() |>
    dplyr::mutate(name = paste0("kegg", 1:5))

  meta_dict <- tibble::tribble(
    ~kegg_id,         ~meta_id, ~type, ~hgnc_symbol,
    "kegg1",          "AAA",    "gene", "1",
    "kegg1",          "BBB",    "gene", "1",
    "kegg2",          "CCC",    "gene", "2",
    "kegg_extra_BBC", "CCC",    "gene", "2",
    "ZZA",            "EEE",    "gene", "4",
    "ZZA",            "EEE",    "gene", "725",
    "kegg4",          "FFF",    "gene", "4",
    "kegg5",          "GGG",    "gene", "5"
  )

  expect_warning(
    g_annot <- annotate_kegg_graph(g, meta_dict, identifiers = "hgnc_symbol"),
    regexp = "Multiple meta_id values found"
  )
})


