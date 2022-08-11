
#' Get Context
#'
#' @param .position
#' A Dataframe produced by position_count()
#' @param .document
#' A Dataframe produced by prep_document()
#' @param .n
#' Number of context token to retrieve, default = NA, which means no token context is retrieved
#' @param .context
#' either "word", "sentence", "paragraph", or "page"
#' @param .vars
#' Any combination of "sentence", "paragraph", and "page" (get the specific locations of hits)\cr
#' If not specified, no variable context is retrieved
#'
#' @return
#' A dataframe
#' @export
#'
#' @examples
#' termlist_short <- prep_termlist(table_termlist_short, string_standardization, TRUE, tid)
#' document       <- prep_document(table_document_short, string_standardization)
#'
#' tab_pos_short <- position_count(
#'   termlist_short, document, sen_id, .cache_terms = TRUE, .tab_pos = NULL
#' )
#'
#' tab_context_word     <- get_context(tab_pos_short, document, 5, "word")
#' tab_context_sentence <- get_context(tab_pos_short, document, 1, "sentence")
#' tab_context_vars     <- get_context(tab_pos_short, document, .vars = "page")

# DEBUG: get_context() ----------------------------------------------------
# .termlist <- prep_termlist(table_termlist_short, string_standardization, TRUE, tid)
# .document <- prep_document(table_document_short, string_standardization)
# .position <- position_count(.termlist, .document, sen_id)
# .n <- 0
# .context <- "sentence"
# .vars = c("sentence", "paragraph", "page")
get_context <- function(
    .position, .document, .n = NA,
    .context = c("word", "sentence", "paragraph", "page"),
    .vars = c("none", "sentence", "paragraph", "page")
) {

  tok_id <- pre <- post <- hash <- hit <- point <- start <- token <- name <-
    term <- doc_id <- n <- NULL

  context_ <- match.arg(.context, c("word", "sentence", "paragraph", "page"))
  vars_ <- stringi::stri_replace_all_fixed(
    .vars, c("sentence", "paragraph", "page"), c("sen_id", "par_id", "pag_id"), FALSE
  )

  quo_ <- switch(context_,
                 "word" = "tok_id",
                 "sentence" = "sen_id",
                 "paragraph" = "par_id",
                 "page" = "pag_id"
  )

  if (!is.na(.n) && context_ == "word" & .n == 0) {
    stop("With .context = word, .n must be greater than 0")
  }


  if (length(unique(.document[[quo_]])) == 1) {
    stop(
      paste0("Document contains only 1 ", context_, ". Not possible to get context.")
    )
  }

  doc_ <- .document %>%
    dplyr::mutate(pre = !!dplyr::sym(quo_), post = !!dplyr::sym(quo_), point = !!dplyr::sym(quo_))

  sep_ <- ifelse(context_ == "word", " ", "<>")


  # Retrieve Token Context --------------------------------------------------
  if (!is.na(.n)) {
    tab_ <- .position %>%
      dplyr::left_join(dplyr::select(doc_, tok_id, pre), by = c("start" = "tok_id")) %>%
      dplyr::left_join(dplyr::select(doc_, tok_id, post), by = c("start" = "tok_id")) %>%
      dplyr::mutate(
        hit = dplyr::row_number(),
        point = purrr::map2(pre, post, ~ (.x - .n):(.y + .n)),
        pre = NULL,
        post = NULL
      ) %>%
      tidyr::unnest(point) %>%
      dtplyr::lazy_dt() %>%
      dplyr::inner_join(dplyr::select(doc_, tok_id, point, token), by = "point") %>%
      dplyr::group_by(hit) %>%
      dplyr::mutate(
        col = dplyr::case_when(
          tok_id >= start & tok_id <= stop ~ "term",
          tok_id < start ~ "pre",
          tok_id > stop ~ "post"
        )
      ) %>%
      dplyr::group_by(doc_id, hash, start, stop, hit, col, point) %>%
      dplyr::summarise(token = paste(token, collapse = " "), .groups = "drop_last") %>%
      dplyr::summarise(token = paste(token, collapse = sep_), n = .n, .groups = "drop") %>%
      tidyr::pivot_wider(names_from = col, values_from = token) %>%
      dplyr::select(hash, start, stop, pre, term, post, n) %>%
      dplyr::arrange(hash, start) %>%
      tibble::as_tibble()
  } else {
    tab_ <- dplyr::select(.position, hash, start, stop)
  }


  # Retrieve Variable Context -----------------------------------------------
  if (!"none" %in% vars_) {
    doc_ <- .document[, c("tok_id", vars_)] %>%
      dplyr::rename(start = tok_id)

    tab_ <- tab_ %>%
      dplyr::left_join(doc_, by = "start") %>%
      dplyr::arrange(hash, start)
  }


  # Retrieve Additional Context ---------------------------------------------
  cn_ <- colnames(.position)
  cn_ <- cn_[!cn_ %in% c("doc_id", "hash", "start", "stop", "dup", "nterm")]


  if (length(cn_) > 0) {
    pos_ <- .position[, cn_]
    dplyr::bind_cols(tibble::as_tibble(tab_), pos_) %>%
      dplyr::mutate(doc_id = .position$doc_id[1]) %>%
      dplyr::select(doc_id, dplyr::everything())
  } else {
    tibble::as_tibble(tab_) %>%
      dplyr::mutate(doc_id = .position$doc_id[1]) %>%
      dplyr::select(doc_id, dplyr::everything())
  }

}



#' Check Termlist
#' @param .tab
#' A Dataframe with at least 1 column (term: Term)
#' @param .fun_std
#' A Function to standardize Strings.\cr
#' Default = NULL (no standardization use)\cr
#' Build-in Function .fun_std = string_standardization, can be used.
#'
#' @param .type
#' Either "warning" or "error", default: "warning"S
#'
#' @return
#' Case 1: No Duplicates: A Message with Dependency Factor (unique terms / all terms)\cr
#' Case 2: A Warning Message and a Dataframe with duplicated values (AFTER STANDARDIZATION)
#' @export
#'
#' @examples
#' # Example w/o standardization
#' check_termlist(table_termlist_short, NULL)
#' # Example with standardization
#' check_termlist(table_termlist_short, string_standardization)
#'
#' # Example w/o standardization and duplicated term
#' termlist <- tibble::add_row(table_termlist_short, term = "Language-Processing")
#' check_termlist(termlist, NULL)
#'
#' # Example with standardization and duplicated term
#' check_termlist(termlist, string_standardization)
check_termlist <- function(.tab, .fun_std = NULL, .type = c("warning", "error")) {

  type_ <- match.arg(.type, c("warning", "error"))
  fmsg <- switch(type_, "warning" = warning, "error" = stop)

  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  source("_debug/debug-check_termlist.R")

  term <- dup_id <- term_orig <- NULL

  # Check Columns in Dataframe -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (!"term" %in% colnames(.tab)) {
    stop("Input MUST contain the column 'term'", call. = FALSE)
  }

  # Standardize Terms -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (!is.null(.fun_std)) {
    tab_ <- dplyr::mutate(.tab, term_orig = term, term = .fun_std(term))
  } else {
    tab_ <- dplyr::mutate(.tab, term_orig = term)
  }

  # Get Duplicates -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  tab_dup_ <- tab_ %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(term) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::mutate(dup_id = dplyr::cur_group_id()) %>%
    dplyr::arrange(dup_id) %>%
    dplyr::ungroup() %>%
    dplyr::select(dup_id, term_orig, term, dplyr::everything()) %>%
    tibble::as_tibble()

  if (nrow(tab_dup_) > 0) {
    fmsg("Termlist contains duplicated terms.", call. = FALSE)
  } else {
    message("Termlist is valid :)")
  }

  return(tab_dup_)
}



#' Find a sequence in another sequence
#'
#' @param .seq_find The sequence you want to find in another sequence
#' @param .seq_base The sequence to be searched
#'
#' @return An integer vector (vector has length zero if sequence is not found)
# # Find an integer sequence
# find_seq_in_seq(2:10, c(1:20, 1:20))
#
# # Find a character sequence
# find_seq_in_seq(c("C", "D"), LETTERS)
#
# # No sequence found
# find_seq_in_seq(c("R", "D"), LETTERS)

find_seq_in_seq <- function(.seq_find, .seq_base) {
  w <- seq_along(.seq_base)

  for (i in seq_along(.seq_find)) {
    w <- w[.seq_base[w + i - 1L] == .seq_find[i]]
    if (length(w) == 0) return(integer(0))
  }

  w <- w[!is.na(w)]
  return(w)
}

#' Helper Function for position_count()
#'
#' @param .row_terms A one row Dataframe prepared by prep_termlist()
#' @param .doc A tokenized Dataframe prepared by prep_document()
#'
#' @return A Dataframe
# DEBUG
# .row_terms <- lst_t_[[1]]
# .doc <- tab_d_
h_position_count <- function(.row_terms, .doc) {
  # Define Variables --------------------------------------------------------
  start <- hash <- ngram <- token <- check <- NULL

  vec_t_ <- unlist(.row_terms[["token"]])
  tab_d_ <- .doc
  len_t_ <- length(vec_t_)

  if (len_t_== 1) {
    tab_pos_ <- tibble::tibble(
      start = which(tab_d_[["token"]] == vec_t_)
    )
  } else {
    tab_pos_ <- tibble::tibble(
      start = find_seq_in_seq(vec_t_, tab_d_[["token"]])
    )
  }

  tab_pos_ %>%
    dplyr::mutate(
      hash   = .row_terms[["hash"]],
      stop  = start + len_t_ - 1L,
      ngram = len_t_,
    ) %>% dplyr::select(hash, ngram, start, stop) %>%
    dplyr::left_join(
      y  = dplyr::mutate(tab_d_, merging_id = dplyr::row_number()),
      by = c("start" = "merging_id")) %>%
    dplyr::select(-token)
}


#' Helper Function: Prepare Termlist
#'
#' @param .tl
#' A Dataframe with at least 1 column (term: Term)
#'
#' @return
#' A Dataframe

# DEBUG
# .tab     <- dplyr::bind_rows(table_termlist_short, table_termlist_long)
# .fun_std <- string_standardization
# quos_    <- dplyr::quos(tid)
tl_prep <- function(.tl) {

  # Define Variables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  token <- hash <- ngram <- term_orig <- oid <- term <- n_dup <- nterm <- NULL

  # Check Columns in Dataframe -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (!"term" %in% colnames(.tab)) {
    stop("Input MUST contain the column 'term'", call. = FALSE)
  }

  if (anyDuplicated(.tab$term)) {
    stop("'term' column must not contain duplicates", call. = FALSE)
  }

  # Prepare Termlist -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  .tab %>%
    dplyr::mutate(
      token = stringi::stri_split_fixed(term, " "),
      hash  = purrr::map_chr(term, ~ digest::digest(.x, algo = "xxhash32")),
      oid   = purrr::map(token, ~ seq_len(length(.x))),
      ngram = lengths(token),
    ) %>%
    dplyr::select(hash, ngram, term, oid, token, dplyr::everything())

}
