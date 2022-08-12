check_cols <- function(.tab, .col) {
  check_ <- .col %in% colnames(.tab)

  if (!check_) {
    msg_ <- glue::glue(".tab must contain column '{.col}'")
    stop(msg_, call. = FALSE)
  }
}

check_dups <- function(.tab, .col, .when = c("before", "after", "none")) {
  when_ <- match.arg(.when, c("before", "after", "none"))
  check_ <- any(duplicated(.tab[[.col]]))

  if (check_ & when_ == "none") {
    msg_ <- glue::glue("Column '{.col}' must not have duplicates")
    stop(msg_, call. = FALSE)
  }

  if (check_ & when_ == "before") {
    msg_ <- glue::glue("Before standardization: Column '{.col}' must not have duplicates")
    stop(msg_, call. = FALSE)
  }

  if (check_ & when_ == "after") {
    msg_ <- glue::glue("After standardization: column '{.col}' must not have duplicates")
    stop(msg_, call. = FALSE)
  }
}

# keep_groups <- function(.tab, .group = c("tok", "oid")) {
#   tok_id <- keep <- oid <- NULL
#
#   if (.group == "tok") {
#     .tab %>%
#       dplyr::mutate(
#         keep = c(1L, diff(tok_id)),
#         keep = dplyr::if_else(dplyr::lead(keep == 1L), 1L, as.integer(keep)),
#         keep = dplyr::if_else(keep == 1L, 1L, NA_integer_),
#         gtok = cumsum(is.na(keep))
#       ) %>%
#       dplyr::filter(!is.na(keep)) %>%
#       dplyr::select(-keep)
#   } else {
#     .tab %>%
#       dplyr::mutate(
#         keep = c(1L, diff(oid)),
#         keep = dplyr::if_else(dplyr::lead(keep == 1L), 1L, as.integer(keep)),
#         keep = dplyr::if_else(keep == 1L, 1L, NA_integer_),
#         goid = cumsum(is.na(keep))
#       ) %>%
#       dplyr::filter(!is.na(keep)) %>%
#       dplyr::select(-keep)
#   }
#
# }



#' Helper Function: Get Term Dependencies
#'
#' @param .tls
#' A Datframe produced by h_prep_termlist()
#'
#' @return
#' A Dataframe
tl_dep <- function(.tls) {

  # Define Variables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  hash <- token <- oid <- sep <- start <- pos <- tok_id <- hash_ <-
    child_pos <- child_hash <- dep <- ngram <- term_orig <- term <-
    children <- parents <- parent_hash <- parent_pos <- NULL

  doc_ <- .tls %>%
    dplyr::select(sep = hash, token, oid) %>%
    tidyr::unnest(c(oid, token)) %>%
    # Columns to use the position_count() function
    dplyr::mutate(tok_id = dplyr::row_number(), doc_id = 1)

  cnt_ <- position_count(.tls, doc_, sep) %>%
    dplyr::mutate(pos = purrr::map2(start, stop, ~ .x:.y)) %>%
    dplyr::select(hash, pos) %>%
    tidyr::unnest(pos) %>%
    dplyr::left_join(dplyr::select(doc_, hash_ = sep, pos = tok_id, oid), by = "pos") %>%
    dplyr::filter(!hash == hash_)


  cnt_children_ <- cnt_ %>%
    dplyr::group_by(hash_, hash) %>%
    dplyr::summarise(child_pos = list(oid), .groups = "drop") %>%
    dplyr::group_by(hash_) %>%
    dplyr::summarise(
      child_hash = list(hash),
      child_pos = list(child_pos),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      children = purrr::map2(child_hash, child_pos, ~ purrr::set_names(c(.y), .x))
    ) %>% dplyr::select(hash = hash_, children)

  cnt_parents_ <- cnt_ %>%
    dplyr::group_by(hash, hash_) %>%
    dplyr::summarise(parent_pos = list(oid), .groups = "drop") %>%
    dplyr::group_by(hash) %>%
    dplyr::summarise(
      parent_hash = list(hash_),
      parent_pos = list(parent_pos),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      parents = purrr::map2(parent_hash, parent_pos, ~ purrr::set_names(c(.y), .x))
    ) %>% dplyr::select(hash, parents)


  .tls %>%
    dplyr::left_join(cnt_children_, by = "hash") %>%
    dplyr::left_join(cnt_parents_, by = "hash") %>%
    dplyr::select(hash, ngram, term, oid, token, parents, children, dplyr::everything())

}

# .tab <- doc_
# .ngram <- 2
get_ngram <- function(.tab, .tls, .ngram) {
  ngram <- hash <- term <- n <- sep <- sep1 <- tok_id <- doc_id <- start <- NULL

  tls_ <- dplyr::filter(.tls, ngram == .ngram)
  tls_ <- dplyr::select(tls_, hash, term, ngram)


  if (.ngram == 1) {
    expr_ <- rlang::parse_expr("paste(token)")
  } else {
    expr_ <- purrr::map_chr(seq_len(.ngram - 1), ~ glue::glue("dplyr::lead(token, {.x})"))
    expr_ <- paste(expr_, collapse = ", ")
    expr_ <- rlang::parse_expr(as.character(glue::glue("paste(token, {expr_})")))
  }

  tab_ <- .tab %>%
    dtplyr::lazy_dt() %>%
    dplyr::filter(n >= .ngram) %>%
    dplyr::mutate(sep1 = dplyr::lead(sep, n = .ngram - 1)) %>%
    dplyr::mutate(term = !!expr_) %>%
    dplyr::inner_join(tls_, by = "term") %>%
    dplyr::filter(sep == sep1) %>%
    dplyr::mutate(start = tok_id, stop = tok_id + .ngram - 1) %>%
    dplyr::select(doc_id, hash, ngram, term, start, stop) %>%
    tibble::as_tibble()
}
