#' Parse KEGG group nodes from a KGML file
#'
#' Extracts group-type entries from a KEGG KGML file and generates both:
#' (1) a mapping of group ID to component node IDs (with meta_id),
#' (2) a set of intra-group edges linking the components as undirected pairs.
#'
#' @param kgml A parsed XML document from `xml2::read_xml()`, representing a KEGG KGML file.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{mapping}{A tibble mapping each member ID to its meta_id (group_id or itself).}
#'   \item{edges}{A tibble with pairwise intra-group edges (from, to, type = "group").}
#' }
#' @importFrom utils combn
#' @export
#'
#' @examples
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' doc <- xml2::read_xml(kgml_file)
#' group_data <- parse_kegg_groups(doc)
#' head(group_data$mapping)
#' head(group_data$edges)
parse_kegg_groups <- function(kgml) {
  group_entries <- xml2::xml_find_all(kgml, "//entry[@type='group']")

  group_components <- purrr::map_dfr(group_entries, function(entry) {
    group_id <- xml2::xml_attr(entry, "id")
    components <- xml2::xml_find_all(entry, ".//component")
    member_ids <- xml2::xml_attr(components, "id")

    tibble::tibble(
      member_id = member_ids,
      meta_id   = group_id
    )
  })

  intra_group_edges <-
    group_components |>
    dplyr::group_by(meta_id) |>
    dplyr::summarise(
      edges = list({
        ids <- member_id
        if (length(ids) > 1) combn(ids, 2, simplify = FALSE) else list()
      }),
      .groups = "drop"
    ) |>
    tidyr::unnest(edges) |>
    dplyr::filter(lengths(edges) == 2) |>
    dplyr::transmute(
      from = purrr::map_chr(edges, 1),
      to   = purrr::map_chr(edges, 2),
      type = "group"
    )

  return(list(
    mapping = group_components,
    edges   = intra_group_edges
  ))
}

#' Parse clean KEGG relations from a KGML file
#'
#' Extracts pairwise relations between KEGG entries and their subtypes if available.
#'
#' @param kgml A parsed XML document from `xml2::read_xml()`.
#'
#' @return A tibble with columns: `from`, `to`, `type`, `subtype`.
#' @export
#'
#' @examples
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' doc <- xml2::read_xml(kgml_file)
#' rels <- parse_kegg_relations_clean(doc)
#' head(rels)
parse_kegg_relations_clean <- function(kgml) {
  relations <- xml2::xml_find_all(kgml, "//relation")

  purrr::map_dfr(relations, function(rel) {
    entry1 <- xml2::xml_attr(rel, "entry1")
    entry2 <- xml2::xml_attr(rel, "entry2")
    type   <- xml2::xml_attr(rel, "type")

    subtype_node <- xml2::xml_find_first(rel, ".//subtype")
    subtype <- if (!is.na(subtype_node)) xml2::xml_attr(subtype_node, "name") else NA

    tibble::tibble(
      from = entry1,
      to   = entry2,
      type = type,
      subtype = subtype
    )
  })
}

#' Combine KEGG group and relation edges into a tidygraph
#'
#' Parses both standard KEGG relations and intra-group edges, and builds a directed tidygraph.
#' Also assigns meta_id to each node: either its group_id (if in a group) or its own ID.
#'
#' @param kgml A parsed XML document from `xml2::read_xml()`.
#'
#' @return A `tidygraph` object with all edges from the KGML and nodes carrying `meta_id`.
#' @export
#'
#' @examples
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' doc <- xml2::read_xml(kgml_file)
#' g <- combine_kegg_network(doc)
#' print(g)
#' igraph::gorder(g)
combine_kegg_network <- function(kgml) {
  edges_rel <- parse_kegg_relations_clean(kgml)
  group_parsed <- parse_kegg_groups(kgml)
  edges_grp <- group_parsed$edges
  mapping    <- group_parsed$mapping

  edges <- dplyr::bind_rows(edges_rel, edges_grp)

  # Node table with meta_id
  all_ids <- unique(c(edges$from, edges$to))
  node_tbl <- tibble::tibble(id = all_ids) |>
    dplyr::left_join(mapping, by = c("id" = "member_id")) |>
    dplyr::mutate(meta_id = dplyr::coalesce(meta_id, id))

  igraph::graph_from_data_frame(edges, vertices = node_tbl, directed = TRUE) |>
    tidygraph::as_tbl_graph()
}
