test_that("parse_kegg_groups returns expected structure and meta_id mapping", {
  kgml <- xml2::read_xml("https://rest.kegg.jp/get/hsa04210/kgml")
  result <- parse_kegg_groups(kgml)

  # Should return a list with two elements
  expect_type(result, "list")
  expect_named(result, c("mapping", "edges"))

  # mapping should be a tibble with member_id and meta_id
  mapping <- result$mapping
  expect_s3_class(mapping, "tbl_df")
  expect_true(all(c("member_id", "meta_id") %in% colnames(mapping)))
  expect_type(mapping$member_id, "character")
  expect_type(mapping$meta_id, "character")
  expect_gt(nrow(mapping), 0)

  # edges should be a tibble with from/to/type
  edges <- result$edges
  expect_s3_class(edges, "tbl_df")
  expect_true(all(c("from", "to", "type") %in% colnames(edges)))
  expect_true(all(edges$type == "group"))
})
