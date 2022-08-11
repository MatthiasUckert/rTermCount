#' Basic String Standardization
#'
#' @param .str A string
#' @param .op Any of c("space", "punct", "case", "ascii")
#'
#' @return A string
#' @export
#'
#' @examples
#' std_str("  TesT String")
std_str <- function(.str, .op = c("space", "punct", "case", "ascii")) {

  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  str_ <- .str

  if ("punct" %in% .op) {
    str_ <- trimws(stringi::stri_replace_all_regex(str_, "\\W", " "))
    str_ <- trimws(stringi::stri_replace_all_regex(str_, "[[:punct:]]", " "))

    if (!"space" %in% .op) {
      str_ <- trimws(stringi::stri_replace_all_regex(str_, "([[:blank:]]|[[:space:]])+", " "))
    }
  }

  if ("space" %in% .op) {
    str_ <- trimws(stringi::stri_replace_all_regex(str_, "([[:blank:]]|[[:space:]])+", " "))
  }

  if ("case" %in% .op) {
    str_ <- tolower(str_)
  }

  if ("ascii" %in% .op) {
    str_ <- stringi::stri_trans_general(str_, "Latin-ASCII")
  }

  return(str_)
}

#' Get Duplicates
#'
#' @param .tab A Term List
#'
#' @return A Dataframe
#' @export
get_dups <- function(.tab) {
  # Assign NULL to Global VAriables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  term <- dup_id <- term_orig <- NULL

  .tab %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(term) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::mutate(dup_id = dplyr::cur_group_id()) %>%
    dplyr::arrange(dup_id) %>%
    dplyr::ungroup() %>%
    dplyr::select(dup_id, term_orig, term, dplyr::everything()) %>%
    tibble::as_tibble()
}

#' Prepare Termlist
#' @param .tab
#' A Dataframe with at least 1 column (term: Term)
#' @param .get_dep
#' Get Term List Dependencies (default = FALSE).\cr
#' Term List Dependencies show how n-grams in the term list are related.
#' @param .fun_std Standardization Function
#'
#' @return
#' A Dataframe with minimum 6 columns:\cr
#' hash: Hashed value (xxhash32) of standardized term\cr
#' ngram: N-Gram of standardized term\cr
#' term_orig: Original term (as defined in parameter .tab)\cr
#' term: Standardized term (with .fun_std)\cr
#' oid: Order ID of token in column token\cr
#' token: Tokienized version of column term\cr
#' dep (optional): Dependencies of terms within the term list\cr
#' ...: Any other column specified by ...
#'
#' @export
prep_termlist <- function(.tab, .fun_std = NULL, .get_dep = FALSE) {

  # Assign NULL to Global VAriables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  term <- token <- hash <- ngram <- term_orig <- oid <- NULL

  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  source("_debug/debug-prep_termlist.R")

  # Checks -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  check_cols(.tab, "term")
  check_dups(.tab, "term", .when = "before")

  # Standardize Terms -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (!is.null(.fun_std)) {
    tab_ <- dplyr::mutate(.tab, term_orig = term, term = .fun_std(term))
  } else {
    tab_ <- dplyr::mutate(.tab, term_orig = term)
  }

  # Checks -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  check_dups(.tab, "term", .when = "after")


  tab_ <- tab_ %>%
    dplyr::mutate(
      token = stringi::stri_split_fixed(term, " "),
      hash  = purrr::map_chr(term, ~ digest::digest(.x, algo = "xxhash32")),
      oid   = purrr::map(token, ~ seq_len(length(.x))),
      ngram = lengths(token),
    ) %>%
    dplyr::select(hash, ngram, term, term_orig, oid, token, dplyr::everything())

  if (.get_dep) tab_ <- tl_dep(tab_)

  return(tab_)
}



#' Prepare Documents
#'
#' @param .tab
#' A Dataframe with at least two columns:\cr
#' doc_id: Unique document identifier\cr
#' text: Document text\cr
#'
#' IMPORTANT NOTE: Dataframe must contain only one row per doc_id
#' @param .fun_std
#' A function to standardize Strings. Default = NULL (no standardization use)
#' @param .until c("tok", "sen", "par", "pag")
#'
#' @return
#' A Dataframe with the following columns:\cr
#' doc_id: Unique document identifier\cr
#' pag_id: Page ID of token\cr
#' par_id: Paragraph ID of token\cr
#' sen_id: Sentence ID of token\cr
#' tok_id: Token ID\cr
#' token: Token (tokenized using white spaces)
#' @export
#'
#' @examples
#' doc <- prep_document(table_doc, std_str)
prep_document <- function(.tab, .fun_std = NULL, .until = c("tok", "sen", "par", "pag")) {

  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # source("_debug/debug-prep_document.R")

  until_ <- match.arg(.until, c("tok", "sen", "par", "pag"))

  # Assign NULL to Global VAriables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  doc_id <- text <- token <- pag_id <- par_id <- sen_id <- tok_id <- NULL

  # Checks -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  check_cols(.tab, "doc_id")
  check_cols(.tab, "text")
  check_dups(.tab, "term", .when = "none")


  # Tokenize Dataframe -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  # To Page -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  tab_ <- .tab %>%
    tidytext::unnest_tokens(
      output = text,
      input = text,
      token = stringi::stri_split_regex, pattern = "\f",
      to_lower = FALSE, drop = FALSE
    ) %>%
    dplyr::mutate(pag_id = dplyr::row_number())

  if (until_ == "pag") {
    if (!is.null(.fun_std)) {
      tab_ <- dplyr::mutate(tab_, text = .fun_std(text))
    }
    tab_ <- dplyr::select(tab_, doc_id, pag_id, token = text)
    return(tab_)
  }


  # To Paragraph -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  tab_ <- tab_ %>%
    tidytext::unnest_tokens(
      output = text,
      input = text,
      token = stringi::stri_split_regex, pattern = "\n+",
      to_lower = FALSE, drop = FALSE
    ) %>%
    dplyr::mutate(par_id = dplyr::row_number())

  if (until_ == "par") {
    if (!is.null(.fun_std)) {
      tab_ <- dplyr::mutate(tab_, text = .fun_std(text))
    }
    tab_ <- dplyr::select(tab_, doc_id, pag_id, par_id, token = text)
    return(tab_)
  }


  # To Sentence -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  tab_ <- tab_ %>%
    tidytext::unnest_tokens(
      output = text,
      input = text,
      token = "sentences",
      to_lower = FALSE
    ) %>%
    dplyr::mutate(sen_id = dplyr::row_number())

  if (until_ == "sen") {
    if (!is.null(.fun_std)) {
      tab_ <- dplyr::mutate(tab_, text = .fun_std(text))
    }
    tab_ <- dplyr::select(tab_, doc_id, pag_id, par_id, sen_id, token = text)
    return(tab_)
  }

  if (!is.null(.fun_std)) {
    tab_ <- dplyr::mutate(tab_, text = .fun_std(text))
  }

  # To Token -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  tab_ <- tab_ %>%
    tidytext::unnest_tokens(
      output = token,
      input = text,
      token = stringi::stri_split_fixed,
      pattern = " ",
      to_lower = FALSE
    ) %>%
    dplyr::filter(!token == "") %>%
    dplyr::mutate(tok_id = dplyr::row_number()) %>%
    dplyr::select(doc_id, pag_id, par_id, sen_id, tok_id, token)

  return(tab_)
}

#' Get Position of Terms
#'
#' @param .tls A term list prepared by prep_termlist()
#' @param .doc A tokenized Dataframe prepared by prep_document()
#' @param ... Any number of Columns that mark a separator of tokens (e.g. a sentence)
#'
#' @return
#' A Dataframe 5 columns:
#' doc_id: Document ID of the dataframe in argument .document prepared by prep_document()\cr
#' hash: Hash value of the terms in argument .termlist prepared by prep_termlist()\cr
#' start: Starting value (tok_id in .document) of the term\cr
#' stop: Ending value (tok_id in .document) of the term\cr\cr
#' dup: Logical indicator if term is contained in higher N-Gram and has already been found
#' @export
#'
# #' @import data.table
position_count <- function(.tls, .doc, ...) {

  # Assign NULL to Global VAriables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  ngram <- token <- doc_id <- hash <- term <- tok_id <- oid <- gtok <- goid <- sep <-
    dup <- sep1 <- sep2 <- check <- start <- ids <- keep <- NULL

  check_cols(.doc, "doc_id")
  check_cols(.doc, "token")
  check_cols(.tls, "term")


  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  source("_debug/debug-position_count.R")

  # Get Quosures -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  quos_     <- dplyr::quos(...)
  quos_vec_ <- sort(gsub("~", "", as.character(unlist(quos_))))

  # Checks -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  check_cols(.doc, "doc_id")
  check_cols(.doc, "token")
  check_cols(.tls, "term")
  check_dups(.tab, "term")
  for (i in quos_vec_) check_cols(.doc, i)


  # Get 1-Grams -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  tls1_ <- dplyr::filter(.tls, ngram == 1)[, c("hash", "term", "ngram")]
  cnt1_ <- .doc %>%
    dplyr::rename(term = token) %>%
    dtplyr::lazy_dt() %>%
    dplyr::inner_join(tls1_, by = "term") %>%
    dplyr::select(doc_id, hash, ngram, term, start = tok_id, stop = tok_id) %>%
    tibble::as_tibble()


  # Get >1-Grams -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  tls2_ <- dplyr::filter(.tls, ngram > 1)
  tls2_ <- tls2_ %>%
    dplyr::select(hash, ngram, token, oid, term) %>%
    tidyr::unnest(c(token, oid))


  # Filter Dataframe for only tokens in vec AND for only consecutive tok_ids
  cnt2_ <- .doc %>%
    # dtplyr::lazy_dt() %>%
    dplyr::filter(token %in% tls2_$token) %>%
    keep_groups(.group = "tok")  %>%
    dplyr::left_join(tls2_, by = "token") %>%
    keep_groups(.group = "oid")  %>%
    dplyr::group_by(gtok, goid) %>%
    dplyr::filter(dplyr::n() == ngram) %>%
    dplyr::mutate(
      sep = paste(!!!quos_, sep = "-"),
      dup = any(duplicated(tok_id))
      ) %>%
    dplyr::summarise(
      doc_id = doc_id[1], hash = hash[1], ngram = ngram[1], term = term[1],
      check = paste(token, collapse = " "),
      start = min(tok_id), stop  = max(tok_id),
      sep1 = dplyr::first(sep), sep2 = dplyr::last(sep),
      dup = any(dup),
      ids = list(tok_id),
      .groups = "drop"
    ) %>%
    dplyr::filter(sep1 == sep2, term == check) %>%
    dplyr::select(-sep1, -sep2, -gtok, -goid, -check) %>%
    tibble::as_tibble()

  cnt3_ <- cnt1_ %>%
    dplyr::mutate(dup = start %in% unlist(cnt2_$ids)) %>%
    dplyr::bind_rows(cnt2_) %>%
    dtplyr::lazy_dt() %>%
    dplyr::arrange(start) %>%
    dplyr::select(-ids) %>%
    tibble::as_tibble()

  return(cnt3_)

}


