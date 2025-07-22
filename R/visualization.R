#' Visualize a KEGG pathway tidygraph using manual layout
#'
#' Generates a `ggraph` plot of a KEGG network with manual layout (x, y),
#' colored by node type and edge subtype. The user can specify which identifier
#' to display on each node label (e.g., `hgnc_symbol`).
#'
#' @param g A `tidygraph` object, where nodes contain `x`, `y`, `type`, and a label column (e.g. `hgnc_symbol`).
#' @param id Unquoted column name to use for node labels (e.g., `hgnc_symbol`, `kegg_id`). Default: `hgnc_symbol`.
#'
#' @return A `ggplot` object with the KEGG network visualization.
#' @export
#'
#' @examples
#' kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
#' dict_file <- system.file("extdata", "example_dict.hsa04210.csv", package = "punKEGGer")
#'
#' kgml <- xml2::read_xml(kgml_file)
#' g <- combine_kegg_network(kgml)
#'
#' node_info <- extract_kegg_nodes(kgml)
#' dict <- read.csv(dict_file)
#' meta_dict <- create_meta_dict(node_info, dict)
#'
#' g_exp <- expand_metagraph(g, node_info, node_types = "gene")
#' g_annot <- annotate_kegg_graph(g_exp, meta_dict, identifiers = "hgnc_symbol")
#'
#' layout_tbl <- layout_kegg(kgml)
#' g_layout <- add_kegg_layout(g_annot, layout_tbl)
#'
#' ggkegg(g_layout, id = hgnc_symbol)
ggkegg <- function(g, id = hgnc_symbol) {
  ggraph::ggraph(g, layout = "manual", x = x, y = y) +

    # Edges
    ggraph::geom_edge_diagonal(
      ggplot2::aes(edge_colour = subtype),
      arrow = grid::arrow(type = "closed", length = ggplot2::unit(5, "pt")),
      lineend = "round",
      strength = 0.7,
      edge_width = 0.4,
      edge_alpha = 0.8,
      end_cap   = ggraph::circle(3, "mm"),
      start_cap = ggraph::circle(1, "mm")
    ) +

    # Nodes, labeled by selected ID
    ggraph::geom_node_label(
      ggplot2::aes(label = {{ id }}, fill = type),
      label.r = ggplot2::unit(0.1, "lines"),
      label.size = 0.5,
      color = "black",
      size = 3,
      label.padding = ggplot2::unit(0.3, "lines"),
      repel = TRUE,
      max.overlaps = Inf
    ) +

    # Node fill colors
    ggplot2::scale_fill_manual(
      values = c(
        gene     = "#BFFFBF",
        compound = "#CFE2F3",
        map      = "#FFE599",
        ortholog = "#F9CB9C",
        group    = "#D9D2E9",
        complex  = "#D9D2E9",
        enzyme   = "#D9EAD3"
      ),
      na.value = "#CCCCCC"
    ) +

    # Edge subtype colors
    ggraph::scale_edge_colour_manual(
      values = c(
        "activation"          = "forestgreen",
        "inhibition"          = "red3",
        "expression"          = "steelblue",
        "binding/association" = "darkorange",
        "missing interaction" = "grey40",
        "phosphorylation"     = "purple"
      ),
      na.value = "black"
    ) +

    ggplot2::theme_void()
}
