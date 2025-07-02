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
    ggplot2::scale_edge_colour_manual(
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
