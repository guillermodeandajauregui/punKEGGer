test_that("parse_kegg_relations_clean extracts KEGG relation edges correctly", {
  kgml <- xml2::read_xml("https://rest.kegg.jp/get/hsa04210/kgml")
  rels <- parse_kegg_relations_clean(kgml)

  # Basic checks
  expect_s3_class(rels, "tbl_df")
  expect_true(all(c("from", "to", "type", "subtype") %in% colnames(rels)))
  expect_gt(nrow(rels), 0)

  # Type should not be missing
  expect_false(any(is.na(rels$type)))

  # From and To should be KEGG entry IDs (usually numeric strings)
  expect_type(rels$from, "character")
  expect_type(rels$to, "character")

  # subtype may contain NA, but if not, should be character
  expect_type(rels$subtype, "character")
})
