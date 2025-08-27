# inst/scripts/make-extdata.R
# -------------------------------------------------------------------
# Provenance of example files in inst/extdata/
# -------------------------------------------------------------------
# Author: Guillermo de Anda Jáuregui
# Date: 2025-08-27
#
# This script documents the origin and generation of the example data
# shipped with punKEGGer for demonstration and testing purposes.
#
# -------------------------------------------------------------------
# 1. hsa04210.xml
# -------------------------------------------------------------------
# - File: inst/extdata/hsa04210.xml
# - Source: Kyoto Encyclopedia of Genes and Genomes (KEGG)
#   Pathway ID: hsa04210 ("Apoptosis")
#   URL: https://www.kegg.jp/pathway/hsa04210
# - Format: KGML (KEGG Markup Language) v0.7.2
# - Downloaded: March 24, 2025 (KEGG web interface)
# - License/terms: Use of KEGG data is subject to the KEGG terms of use.
#   Redistribution of full database is not permitted; this example is a
#   single pathway XML file used here solely for illustration and testing.
#
# -------------------------------------------------------------------
# 2. example_dict.hsa04210.csv
# -------------------------------------------------------------------
# - File: inst/extdata/example_dict.hsa04210.csv
# - Source: Ensembl BioMart (Ensembl release 112, March 2025)
# - Organism: Homo sapiens (GRCh38)
# - Attributes selected:
#     * KEGG gene ID
#     * HGNC symbol
#     * Ensembl Gene ID
# - Method:
#   The BioMart web interface (https://www.ensembl.org/biomart/) was used
#   to query human genes with KEGG mappings. The result was exported to CSV
#   and manually filtered to the set of genes present in pathway hsa04210.
#
# -------------------------------------------------------------------
# Notes:
# -------------------------------------------------------------------
# These files are provided only as small, reproducible examples for the
# package vignette and test suite. Users are expected to download their
# own KEGG pathways and generate dictionaries from BioMart or similar
# sources for analysis.
# -------------------------------------------------------------------

