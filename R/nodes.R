#' Extract KEGG node information from a KGML file
#'
#' Parses all `entry` elements from a KEGG KGML XML document and extracts ID, name,
#' type, and individual KEGG IDs. Supports multi-gene entries (e.g., `hsa:1234+hsa:5678`)
#' by expanding them into separate rows with a shared `meta_id`.
#'
#' @param kgml A parsed XML document from `xml2::read_xml()`.
#'
#' @return A tibble with columns: `id`, `meta_id`, `type`, `kegg_id`
#' @export
#'
#' @examples
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' doc <- xml2::read_xml(kgml_file)
#' nodes <- extract_kegg_nodes(doc)
#' head(nodes)
extract_kegg_nodes <- function(kgml) {
  entries <- xml2::xml_find_all(kgml, "//entry")

  tibble::tibble(
    id   = xml2::xml_attr(entries, "id"),
    name = xml2::xml_attr(entries, "name"),
    type = xml2::xml_attr(entries, "type")
  ) |>
    dplyr::mutate(meta_id = id) |>
    dplyr::mutate(kegg_ids = stringr::str_split(name, "\\+")) |>
    tidyr::unnest(kegg_ids) |>
    dplyr::mutate(
      kegg_id = stringr::str_trim(kegg_ids),
      id = as.character(id)
    ) |>
    dplyr::select(id, meta_id, type, kegg_id)
}

#' Expand a KEGG tidygraph by unrolling multi-gene metanodes
#'
#' Takes a tidygraph where node names are KGML entry IDs and expands the network
#' by replacing group/multi-gene nodes with individual gene-level edges.
#'
#' @param g A `tidygraph` object, typically created by `combine_kegg_network()`.
#' @param node_info A tibble as returned by `extract_kegg_nodes()`.
#' @param node_types Character vector indicating which node types to expand (default is `"gene"`).
#'
#' @return A new `tidygraph` object with expanded node IDs (e.g., `hsa:00001`, `hsa:00002`).
#' @importFrom tidyselect everything
#' @export
#'
#' @examples
#' # Parse KGML and extract nodes and relations
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' doc <- xml2::read_xml(kgml_file)
#' node_info <- extract_kegg_nodes(doc)
#' rels <- parse_kegg_relations_clean(doc)
#' g <- combine_kegg_network(node_info, rels)
#'
#' # Expand the graph from metanodes to gene-level nodes
#' g_exp <- expand_metagraph(g, node_info)
#'
#' # Compare number of nodes before and after expansion
#' igraph::gorder(g)      # number of metanodes
#' igraph::gorder(g_exp)  # number of gene-level nodes
expand_metagraph <- function(g, node_info, node_types = c("gene")) {
  edges <-
    tidygraph::as_tibble(g, active = "edges") |>
    dplyr::mutate(from = as.character(from), to = as.character(to))

  edge_debug_from <-
    edges |>
    dplyr::left_join(
      node_info |> dplyr::select(id, kegg_id, type) |> dplyr::rename(kegg_id_from = kegg_id, type_from = type),
      by = c("from" = "id")
    )

  edge_debug_full <-
    edge_debug_from |>
    dplyr::left_join(
      node_info |> dplyr::select(id, kegg_id, type) |> dplyr::rename(kegg_id_to = kegg_id, type_to = type),
      by = c("to" = "id")
    )

  edge_debug_full |>
    tidyr::drop_na() |>
    dplyr::filter(type_from %in% node_types, type_to %in% node_types) |>
    tidyr::separate_longer_delim(kegg_id_from, delim = " ") |>
    tidyr::separate_longer_delim(kegg_id_to, delim = " ") |>
    dplyr::rename(
      from_legacy = from,
      to_legacy   = to,
      from        = kegg_id_from,
      to          = kegg_id_to
    ) |>
    dplyr::select(from, to, tidyselect::everything()) |>
    igraph::graph_from_data_frame() |>
    tidygraph::as_tbl_graph()
}
