## code to prepare `test_document` dataset goes here
library(tidyverse)
table_doc <- readtext::readtext("data-raw/the_importance_of_being_on_twitter.txt") %>%
  mutate(doc_id = gsub(".txt", "", doc_id)) %>%
  tibble::as_tibble()

table_terms <- openxlsx::read.xlsx("data-raw/expected_term_counts.xlsx") %>%
  mutate(tid = row_number()) %>%
  tibble::as_tibble()

table_check <- table_terms %>%
  select(tid, term, n1, n2) %>%
  tibble::as_tibble()

table_terms <- table_terms %>%
  select(tid, term) %>%
  mutate(origin = "GPT3") %>%
  tibble::as_tibble()

usethis::use_data(table_doc, overwrite = TRUE)
usethis::use_data(table_terms, overwrite = TRUE)
usethis::use_data(table_check, overwrite = TRUE)
