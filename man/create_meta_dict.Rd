% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotation.R
\name{create_meta_dict}
\alias{create_meta_dict}
\title{Create a meta_id-to-annotation dictionary}
\usage{
create_meta_dict(node_info, kegg_dict)
}
\arguments{
\item{node_info}{A tibble from \code{extract_kegg_nodes()} with \code{meta_id} and \code{kegg_id} columns.}

\item{kegg_dict}{A tibble mapping KEGG gene IDs to external identifiers.
Must contain a column \code{kegg_id} with KEGG-formatted IDs (e.g., \code{"hsa:1234"}).}
}
\value{
A tibble linking each \code{meta_id} to any identifiers in \code{kegg_dict}.
}
\description{
Takes the output of \code{extract_kegg_nodes()} and a KEGG annotation dictionary
(e.g., from BioMart or custom source), and merges them based on \code{kegg_id}.
This enables annotating each metanode (\code{meta_id}) with HGNC, Ensembl, Entrez, etc.
}
\examples{
kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
dict_file <- system.file("extdata", "example_dict.hsa04210.csv", package = "punKEGGer")

doc <- xml2::read_xml(kgml_file)
nodes <- extract_kegg_nodes(doc)
dict <- read.csv(dict_file)
meta_dict <- create_meta_dict(nodes, dict)
head(meta_dict)
}
