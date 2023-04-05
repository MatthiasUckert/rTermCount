## code to prepare `test_document` dataset goes here
library(tidyverse)
table_doc <- readtext::readtext("data-raw/vch-sustainability-value-chain-basf-ar21.pdf") %>%
  dplyr::mutate(doc_id = "BASF ESG") %>%
  tibble::as_tibble()


check_for_pert_of_ngram <- function(.term, .vec) {
  vec_ <- .vec
  vec_[.vec == .term] <- ""
  paste(which(grepl(paste0("\\b", .term, "\\b"), vec_)), collapse = ", ")
}


table_terms <- openxlsx::read.xlsx("data-raw/esg_terms.xlsx") %>%
  dplyr::mutate(tmp = rTermCount::std_str(term)) %>%
  dplyr::mutate(
    part_of_ngram = purrr::map_chr(tmp, ~ check_for_pert_of_ngram(.x, tmp)),
    part_of_ngram = dplyr::if_else(part_of_ngram == "", NA_character_, part_of_ngram)
  ) %>%
  dplyr::select(tid, term, part_of_ngram, origin = source) %>%
  tibble::as_tibble()

usethis::use_data(table_doc, overwrite = TRUE)
usethis::use_data(table_terms, overwrite = TRUE)
