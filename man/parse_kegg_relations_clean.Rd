% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_kegg.R
\name{parse_kegg_relations_clean}
\alias{parse_kegg_relations_clean}
\title{Parse clean KEGG relations from a KGML file}
\usage{
parse_kegg_relations_clean(kgml)
}
\arguments{
\item{kgml}{A parsed XML document from \code{xml2::read_xml()}.}
}
\value{
A tibble with columns: \code{from}, \code{to}, \code{type}, \code{subtype}.
}
\description{
Extracts pairwise relations between KEGG entries and their subtypes if available.
}
\examples{
kgml_file <- system.file("extdata", "hsa04210.xml", package = "punKEGGer")
doc <- xml2::read_xml(kgml_file)
rels <- parse_kegg_relations_clean(doc)
head(rels)
}
