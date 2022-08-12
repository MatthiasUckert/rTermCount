check_cols <- function(.tab, .col, .type = c(".doc", ".tls")) {
  check_ <- .col %in% colnames(.tab)

  if (!check_) {
    msg_ <- glue::glue("{.type} must contain column '{.col}'")
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

get_ngram <- function(.tab, .tls, .ngram) {

  ngram <- tid <- term <- n <- sep <- sep1 <- tok_id <- doc_id <- start <- NULL

  tls_ <- dplyr::filter(.tls, ngram == .ngram)
  tls_ <- dplyr::select(tls_, tid, term, ngram)


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
    dplyr::select(doc_id, tid, ngram, term, start, stop) %>%
    tibble::as_tibble()
}




# tl_dep <- function(.tls) {
#
#   # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
#   source("_debug/debug-tl_dep.R")
#
#   doc_ <- .tls %>%
#     dplyr::mutate(token = stringi::stri_split_fixed(term, " ")) %>%
#     dplyr::mutate(doc_id = 1) %>%
#     dplyr::select(doc_id, tid, token) %>%
#     tidyr::unnest(token) %>%
#     dplyr::mutate(tok_id = dplyr::row_number()) %>%
#     dplyr::group_by(sep = tid) %>%
#     dplyr::mutate(n = dplyr::n()) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-tid)
#
#   cnt_ <- position_count(.tls = .tls, .doc = doc_, sep) %>%
#     dplyr::mutate(pos = purrr::map2(start, stop, ~ .x:.y)) %>%
#     dplyr::select(tid, pos) %>%
#     tidyr::unnest(pos) %>%
#     dplyr::left_join(dplyr::select(doc_, hash_ = sep, pos = tok_id, oid), by = "pos") %>%
#     dplyr::filter(!hash == hash_)
#
#
#   cnt_children_ <- cnt_ %>%
#     dplyr::group_by(hash_, hash) %>%
#     dplyr::summarise(child_pos = list(oid), .groups = "drop") %>%
#     dplyr::group_by(hash_) %>%
#     dplyr::summarise(
#       child_hash = list(hash),
#       child_pos = list(child_pos),
#       .groups = "drop"
#     ) %>%
#     dplyr::mutate(
#       children = purrr::map2(child_hash, child_pos, ~ purrr::set_names(c(.y), .x))
#     ) %>% dplyr::select(hash = hash_, children)
#
#   cnt_parents_ <- cnt_ %>%
#     dplyr::group_by(hash, hash_) %>%
#     dplyr::summarise(parent_pos = list(oid), .groups = "drop") %>%
#     dplyr::group_by(hash) %>%
#     dplyr::summarise(
#       parent_hash = list(hash_),
#       parent_pos = list(parent_pos),
#       .groups = "drop"
#     ) %>%
#     dplyr::mutate(
#       parents = purrr::map2(parent_hash, parent_pos, ~ purrr::set_names(c(.y), .x))
#     ) %>% dplyr::select(hash, parents)
#
#
#   .tls %>%
#     dplyr::left_join(cnt_children_, by = "hash") %>%
#     dplyr::left_join(cnt_parents_, by = "hash") %>%
#     dplyr::select(hash, ngram, term, oid, token, parents, children, dplyr::everything())
#
# }
