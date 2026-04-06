test_that("fetch_kegg_kgml input validation works", {

  expect_error(
    fetch_kegg_kgml(NA_character_),
    "`pathway_id` must be a single non-missing character string."
  )

  expect_error(
    fetch_kegg_kgml(c("hsa04210", "hsa04010")),
    "`pathway_id` must be a single non-missing character string."
  )

})
