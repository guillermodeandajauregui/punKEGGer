test_that("KGML without groups returns typed tables and retains relations", {
  kgml <- xml2::read_xml(testthat::test_path("fixtures", "metanodes.xml"))
  expect_identical(parse_kegg_groups(kgml), list(
    mapping = tibble::tibble(member_id = character(), meta_id = character()),
    edges = tibble::tibble(from = character(), to = character(), type = character())
  ))

  graph <- combine_kegg_network(kgml)
  nodes <- tidygraph::as_tibble(graph)
  expect_identical(nodes$meta_id, nodes$name)
  expect_equal(igraph::gsize(graph), 3)
})

test_that("expansion preserves KGML endpoints, optional subtypes and namespaces", {
  kgml <- xml2::read_xml(testthat::test_path("fixtures", "metanodes.xml"))
  nodes <- extract_kegg_nodes(kgml)
  expect_identical(nodes$kegg_id, c("hsa:1", "hsa:2", "hsa:3", "hsa:4", "path:hsa04910"))

  graph <- combine_kegg_network(kgml)
  expanded <- expand_metagraph(graph, nodes, node_types = c("gene", "map"))
  edges <- igraph::as_data_frame(expanded, what = "edges")
  expected <- c(
    "hsa:1 hsa:3", "hsa:1 hsa:4", "hsa:2 hsa:3", "hsa:2 hsa:4",
    "hsa:3 hsa:1", "hsa:3 hsa:2", "hsa:4 hsa:1", "hsa:4 hsa:2",
    "hsa:3 path:hsa04910", "hsa:4 path:hsa04910"
  )
  expect_equal(sort(paste(edges$from, edges$to)), sort(expected))
  expect_identical(edges$from_legacy, rep(c("40", "10"), c(4, 6)))
  expect_identical(edges$to_legacy, rep(c("10", "40", "90"), c(4, 4, 2)))
  expect_equal(sum(is.na(edges$subtype)), 6)
  expect_equal(igraph::gsize(expand_metagraph(graph, nodes)), 8)

  legacy_nodes <- nodes[c(1, 3, 5), ]
  legacy_nodes$kegg_id[1:2] <- c("hsa:1 hsa:2", "hsa:3+hsa:4")
  legacy_expanded <- expand_metagraph(graph, legacy_nodes, node_types = c("gene", "map"))
  expect_identical(igraph::as_data_frame(legacy_expanded, what = "edges"), edges)

  dict <- tibble::tibble(kegg_id = nodes$kegg_id, label = nodes$kegg_id)
  meta_dict <- create_meta_dict(nodes, dict)
  expect_identical(create_meta_dict(legacy_nodes, dict), meta_dict)
  annotated <- annotate_kegg_graph(expanded, meta_dict, identifiers = "label")
  laid_out <- add_kegg_layout(annotated, layout_kegg(kgml))
  plotted_nodes <- tidygraph::as_tibble(laid_out)
  expect_identical(plotted_nodes$label, plotted_nodes$name)
  expect_identical(plotted_nodes$x, c(10, 10, 30, 30, 50))
  expect_identical(plotted_nodes$y, c(20, 20, 40, 40, 60))
  built <- ggplot2::ggplot_build(plot_kegg_graph(laid_out, id = label))
  expect_identical(built$data[[2]]$label, plotted_nodes$label)
})

test_that("intra-group edges survive expansion without a relation subtype", {
  kgml <- xml2::read_xml('
    <pathway>
      <entry id="20" name="hsa:1" type="gene"/>
      <entry id="30" name="hsa:2" type="gene"/>
      <entry id="60" name="undefined" type="group">
        <component id="20"/>
        <component id="30"/>
      </entry>
    </pathway>
  ')
  graph <- combine_kegg_network(kgml)
  expanded <- expand_metagraph(graph, extract_kegg_nodes(kgml))
  edges <- igraph::as_data_frame(expanded, what = "edges")
  expect_equal(edges$from, "hsa:1")
  expect_equal(edges$to, "hsa:2")
  expect_equal(edges$type, "group")
  expect_identical(edges$subtype, NA_character_)
})
