% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{ggkegg}
\alias{ggkegg}
\title{Visualize a KEGG pathway tidygraph using manual layout}
\usage{
ggkegg(g, id = hgnc_symbol)
}
\arguments{
\item{g}{A \code{tidygraph} object, where nodes contain \code{x}, \code{y}, \code{type}, and a label column (e.g. \code{hgnc_symbol}).}

\item{id}{Unquoted column name to use for node labels (e.g., \code{hgnc_symbol}, \code{kegg_id}). Default: \code{hgnc_symbol}.}
}
\value{
A \code{ggplot} object with the KEGG network visualization.
}
\description{
Generates a \code{ggraph} plot of a KEGG network with manual layout (x, y),
colored by node type and edge subtype. The user can specify which identifier
to display on each node label (e.g., \code{hgnc_symbol}).
}
