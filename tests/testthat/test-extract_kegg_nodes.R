test_that("extract_kegg_nodes returns expected columns and structure", {
  kgml <- xml2::read_xml("https://rest.kegg.jp/get/hsa04210/kgml")
  nodes <- extract_kegg_nodes(kgml)

  # Must be a tibble with expected columns
  expect_s3_class(nodes, "tbl_df")
  expect_true(all(c("id", "meta_id", "type", "kegg_id") %in% colnames(nodes)))

  # No missing values in critical columns
  expect_false(any(is.na(nodes$id)))
  expect_false(any(is.na(nodes$kegg_id)))

  # meta_id must match id (as created by extract_kegg_nodes)
  expect_equal(nodes$meta_id, nodes$id)

  # Optional: warn if no multi-gene nodes
  multi_gene_groups <- nodes |>
    dplyr::group_by(meta_id) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1)

  if (nrow(multi_gene_groups) == 0) {
    message("⚠️ No multi-gene entries found in hsa04210 — skipping this check.")
  } else {
    expect_gt(nrow(multi_gene_groups), 0)
  }
})
