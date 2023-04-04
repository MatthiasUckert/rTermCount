## code to prepare `test_document` dataset goes here
library(tidyverse)
table_doc <- readtext::readtext("data-raw/vch-sustainability-value-chain-basf-ar21.pdf") %>%
  dplyr::mutate(doc_id = "BASF ESG") %>%
  dplyr::rename(origin = source) %>%
  tibble::as_tibble()

table_terms <- openxlsx::read.xlsx("data-raw/esg_terms.xlsx") %>%
  tibble::as_tibble()

usethis::use_data(table_doc, overwrite = TRUE)
usethis::use_data(table_terms, overwrite = TRUE)
