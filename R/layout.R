#' Extract node layout coordinates from a KEGG KGML file
#'
#' Parses the `graphics` element of each node in the KGML to extract manual layout
#' positions (x, y), which are typically defined for KEGG maps.
#'
#' @param kgml A parsed XML document from `xml2::read_xml()`.
#'
#' @return A tibble with columns: `id`, `x`, `y` (corresponding to KEGG entry IDs).
#' @export
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
add_kegg_layout <- function(graph, layout_tbl) {
  graph |>
    tidygraph::activate("nodes") |>
    dplyr::left_join(layout_tbl, by = c("meta_id" = "id"))
}
