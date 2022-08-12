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
#' @param .get_dep NOt yeat implemented
#' @param .fun_std Standardization Function
#'
#' @return A Dataframe
#'
#' @export
prep_termlist <- function(.tab, .fun_std = NULL, .get_dep = FALSE) {

  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # source("_debug/debug-prep_termlist.R")

  # Assign NULL to Global VAriables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  term <- token <- tid <- ngram <- term_orig <- oid <- NULL

  # Checks -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  check_cols(.tab, "term", .type = ".doc")
  check_cols(.tab, "tid", .type = ".doc")
  check_dups(.tab, "term", .when = "before")
  check_dups(.tab, "tid", .when = "before")


  # Standardize Terms -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (!is.null(.fun_std)) {
    tab_ <- dplyr::mutate(.tab, term_orig = term, term = .fun_std(term))
  } else {
    tab_ <- dplyr::mutate(.tab, term_orig = term)
  }

  # Checks -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  check_dups(.tab, "term", .when = "after")

  tab_ <- tab_ %>%
    dplyr::mutate(ngram = stringi::stri_count_fixed(term, " ") + 1L) %>%
    dplyr::select(tid, ngram, term, term_orig, dplyr::everything())

  # if (.get_dep) tab_ <- tl_dep(tab_)

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

  # Assign NULL to Global VAriables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  doc_id <- text <- token <- pag_id <- par_id <- sen_id <- tok_id <- NULL

  until_ <- match.arg(.until, c("tok", "sen", "par", "pag"))

  # Checks -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  check_cols(.tab, "doc_id", .type = ".doc")
  check_cols(.tab, "text", .type = ".doc")
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
#' tid: Hash value of the terms in argument .termlist prepared by prep_termlist()\cr
#' start: Starting value (tok_id in .document) of the term\cr
#' stop: Ending value (tok_id in .document) of the term\cr
#' term: Term\cr
#' dup: Logical indicator if term is contained in higher N-Gram and has already been found
#' @export
#'
#' @import data.table
position_count <- function(.tls, .doc, ...) {

  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # source("_debug/debug-position_count.R")

  # Assign NULL to Global VAriables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  ngram <- token <- doc_id <- hash <- term <- tok_id <- oid <- gtok <- goid <- sep <-
    dup <- sep1 <- sep2 <- check <- start <- ids <- keep <- NULL

  # Get Quosures -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  quos_     <- dplyr::quos(...)
  quos_vec_ <- sort(gsub("~", "", as.character(unlist(quos_))))

  # Checks (Document) -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  check_cols(.doc, "doc_id", .type = ".doc")
  check_cols(.doc, "token", .type = ".doc")
  for (i in quos_vec_) check_cols(.doc, i, .type = ".doc")


  # Checks (Termlist) -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  check_cols(.tls, "tid", .type = ".doc")
  check_cols(.tls, "term", .type = ".doc")
  check_dups(.tls, "term")



  # Prepare Document -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  doc_ <- .doc %>%
    dplyr::group_by(!!!quos_) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::mutate(sep = paste(!!!quos_, sep = "-")) %>%
    dplyr::ungroup()


  # Calculate Ngram -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  ngrams_ <- sort(unique(.tls$ngram))
  purrr::map_dfr(ngrams_, ~ get_ngram(doc_, .tls, .x)) %>%
    dplyr::arrange(start, -ngram) %>%
    dplyr::mutate(
      ids = purrr::map2(start, stop, ~ .x:.y),
      dup = utils::relist(flesh = duplicated(unlist(ids)), skeleton = ids),
      dup = purrr::map_lgl(dup, all)
    ) %>%
    dplyr::select(-ids)
}


