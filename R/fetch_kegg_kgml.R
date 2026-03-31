#' Fetch a KEGG pathway KGML via KEGGREST
#'
#' Downloads a KEGG pathway in KGML format using `KEGGREST::keggGet()`
#' and returns it as an `xml2` document ready for downstream parsing with
#' `punKEGGer`.
#'
#' @param pathway_id A KEGG pathway identifier, e.g. `"hsa04210"`.
#'
#' @return An `xml2` document.
#' @export
#'
#' @examples
#' \dontrun{
#' kgml <- fetch_kegg_kgml("hsa04210")
#' g <- combine_kegg_network(kgml)
#' }
fetch_kegg_kgml <- function(pathway_id) {
  if (!requireNamespace("KEGGREST", quietly = TRUE)) {
    rlang::abort(
      paste(
        "Package 'KEGGREST' is required for fetch_kegg_kgml().",
        "Please install it with BiocManager::install('KEGGREST')."
      )
    )
  }

  if (!is.character(pathway_id) || length(pathway_id) != 1 || is.na(pathway_id)) {
    rlang::abort("`pathway_id` must be a single non-missing character string.")
  }

  kgml_raw <-
    tryCatch(
      KEGGREST::keggGet(pathway_id, "kgml"),
      error = function(e) {
        rlang::abort(
          paste0(
            "Failed to retrieve KGML for pathway '",
            pathway_id,
            "'. KEGGREST error: ",
            conditionMessage(e)
          )
        )
      }
    )

  if (length(kgml_raw) == 0 || is.null(kgml_raw[[1]])) {
    rlang::abort(
      paste0("No KGML content was returned for pathway '", pathway_id, "'.")
    )
  }

  kgml_obj <-
    kgml_raw[[1]]

  if (inherits(kgml_obj, "xml_document")) {
    return(kgml_obj)
  }

  if (inherits(kgml_obj, "XMLInternalDocument")) {
    return(
      xml2::read_xml(
        XML::saveXML(kgml_obj)
      )
    )
  }

  if (is.raw(kgml_obj)) {
    return(
      xml2::read_xml(
        rawToChar(kgml_obj)
      )
    )
  }

  if (is.character(kgml_obj) && length(kgml_obj) == 1) {
    return(
      xml2::read_xml(kgml_obj)
    )
  }

  rlang::abort(
    paste0(
      "Unsupported KGML object returned by KEGGREST for pathway '",
      pathway_id,
      "'."
    )
  )
}
