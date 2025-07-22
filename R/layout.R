#' Extract node layout coordinates from a KEGG KGML file
#'
#' Parses the `graphics` element of each node in the KGML to extract manual layout
#' positions (x, y), which are typically defined for KEGG maps.
#'
#' @param kgml A parsed XML document from `xml2::read_xml()`.
#'
#' @return A tibble with columns: `id`, `x`, `y` (corresponding to KEGG entry IDs).
#' @export
#'
#' @examples
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' doc <- xml2::read_xml(kgml_file)
#' layout_tbl <- layout_kegg(doc)
#' head(layout_tbl)
layout_kegg <- function(kgml) {
  node_entries <- xml2::xml_find_all(kgml, ".//entry")

  purrr::map_dfr(node_entries, function(node) {
    id <- xml2::xml_attr(node, "id")
    graphics_node <- xml2::xml_find_first(node, "graphics")

    if (!is.na(graphics_node)) {
      x <- as.numeric(xml2::xml_attr(graphics_node, "x"))
      y <- as.numeric(xml2::xml_attr(graphics_node, "y"))
      tibble::tibble(id = id, x = x, y = y)
    } else {
      tibble::tibble()
    }
  })
}

#' Add KEGG layout coordinates to a tidygraph
#'
#' Joins a KEGG layout table (as returned by `layout_kegg()`) to a tidygraph object,
#' using the `meta_id` field of each node as the key.
#'
#' @param graph A `tidygraph` object, where each node has a `meta_id`.
#' @param layout_tbl A tibble with layout coordinates: `id`, `x`, `y`.
#'
#' @return The same `tidygraph`, with `x` and `y` coordinates added to nodes.
#' @export
#'
#' @examples
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' doc <- xml2::read_xml(kgml_file)
#'
#' g <- combine_kegg_network(doc)
#' layout_tbl <- layout_kegg(doc)
#' g_layout <- add_kegg_layout(g, layout_tbl)
#'
#' # View the layout coordinates
#' head(tidygraph::as_tibble(g_layout))
add_kegg_layout <- function(graph, layout_tbl) {
  graph |>
    tidygraph::activate("nodes") |>
    dplyr::left_join(layout_tbl, by = c("meta_id" = "id"))
}
