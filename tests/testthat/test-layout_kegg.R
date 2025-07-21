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
