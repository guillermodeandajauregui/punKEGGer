#' Create a meta_id-to-annotation dictionary
#'
#' Takes the output of `extract_kegg_nodes()` and a KEGG annotation dictionary
#' (e.g., from BioMart or custom source), and merges them based on `kegg_id`.
#' This enables annotating each metanode (`meta_id`) with HGNC, Ensembl, Entrez, etc.
#'
#' @param node_info A tibble from `extract_kegg_nodes()` with `meta_id` and `kegg_id` columns.
#' @param kegg_dict A tibble mapping KEGG gene IDs to external identifiers.
#' Must contain a column `kegg_id` with KEGG-formatted IDs (e.g., `"hsa:1234"`).
#'
#' @return A tibble linking each `meta_id` to any identifiers in `kegg_dict`.
#' @export
#'
#' @examples
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' dict_file <- system.file("extdata", "example_dict.hsa04210.csv", package = "punKEGGer")
#'
#' doc <- xml2::read_xml(kgml_file)
#' nodes <- extract_kegg_nodes(doc)
#' dict <- readr::read_csv(dict_file, show_col_types = FALSE)
#'
#' meta_dict <- create_meta_dict(nodes, dict)
#' head(meta_dict)
create_meta_dict <- function(node_info, kegg_dict) {
  node_info_expanded <-
    node_info |>
    tidyr::separate_longer_delim(cols = kegg_id, delim = " ") |>
    dplyr::select(-id)  # keep only meta_id and kegg_id

  meta_dict <-
    dplyr::inner_join(node_info_expanded, kegg_dict, by = "kegg_id")

  return(meta_dict)
}

#' Annotate a KEGG tidygraph with custom identifiers
#'
#' Adds external identifiers (e.g., HGNC symbols, Ensembl IDs) to the nodes of a
#' `tidygraph`-style KEGG network, using a custom dictionary.
#'
#' **Note:** You must provide your own annotation dictionary (`kegg_dict`) as a
#' tibble with at least one column named `kegg_id`, containing KEGG gene IDs in
#' the form `"hsa:1234"`. This dictionary can be generated from BioMart or other
#' curated sources.
#'
#' @param graph A tidygraph object (nodes should be named with KEGG IDs).
#' @param kegg_dict A tibble with one column `kegg_id`, plus any other IDs (e.g. `hgnc_symbol`, `ensembl_id`, etc.).
#' @param identifiers Character vector of column names in `kegg_dict` to attach (default: `"hgnc_symbol"`).
#'
#' @return The same tidygraph, with new columns added to the node attributes.
#' @export
#'
#' @examples
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' dict_file <- system.file("extdata", "example_dict.hsa04210.csv", package = "punKEGGer")
#'
#' doc <- xml2::read_xml(kgml_file)
#' dict <- readr::read_csv(dict_file, show_col_types = FALSE)
#' g <- combine_kegg_network(doc)
#' g_annot <- annotate_kegg_graph(g, dict)
#' head(tidygraph::as_tibble(g_annot))
annotate_kegg_graph <- function(graph, kegg_dict, identifiers = c("hgnc_symbol")) {
  dict_cols <- colnames(kegg_dict)
  valid_ids <- identifiers[identifiers %in% dict_cols]
  invalid_ids <- setdiff(identifiers, dict_cols)

  if (length(valid_ids) == 0) {
    rlang::abort(glue::glue("
[ERROR] None of the requested identifiers were found in your dictionary.
Requested: {toString(identifiers)}
Available columns: {toString(dict_cols)}
    "))
  }

  if (length(invalid_ids) > 0) {
    warning(glue::glue("
[WARNING] Some requested identifiers are not present in the dictionary and will be ignored.
Missing: {toString(invalid_ids)}
Using: {toString(valid_ids)}
    "))
  }

  # Warn if same KEGG ID maps to multiple meta_id
  multi_meta <-
    kegg_dict |>
    dplyr::distinct(kegg_id, meta_id) |>
    dplyr::group_by(kegg_id) |>
    dplyr::filter(dplyr::n() > 1)

  if (nrow(multi_meta) > 0) {
    warning(glue::glue("
[WARNING] Multiple meta_id values found for {nrow(multi_meta)} KEGG IDs.
Only the first meta_id per KEGG ID will be used.
You should probably go check your dictionary, punk.
"))
  }

  collapsed_dict <-
    kegg_dict |>
    dplyr::group_by(kegg_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  node_dict <-
    collapsed_dict |>
    dplyr::filter(kegg_id %in% igraph::V(graph)$name) |>
    dplyr::select(name = kegg_id, meta_id, type, dplyr::all_of(valid_ids))

  multi_match <-
    node_dict |>
    dplyr::count(name) |>
    dplyr::filter(n > 1)
  # this section commented because seems unneeded with current flow
  # but could be useful to ensure data shape
  #   if (nrow(multi_match) > 0) {
  #     warning(glue::glue("
  # [WARNING] Multiple matches found for {nrow(multi_match)} KEGG IDs when trying to annotate with {toString(valid_ids)}.
  # Only the first match per ID will be used.
  # You should probably go check your dictionary, punk.
  # "))
  #     node_dict |>
  #       dplyr::group_by(name) |>
  #       dplyr::slice(1) |>
  #       dplyr::ungroup()
  #   }

  graph |>
    tidygraph::activate("nodes") |>
    dplyr::left_join(node_dict, by = "name")
}
