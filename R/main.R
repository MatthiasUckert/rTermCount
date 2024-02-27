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
      to_lower = FALSE,
      drop = FALSE
    ) %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(doc_id) %>%
    dplyr::mutate(pag_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()

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
      token = stringi::stri_split_regex, pattern = "\n{2,}",
      to_lower = FALSE,
      drop = FALSE
    ) %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(doc_id) %>%
    dplyr::mutate(par_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()

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
      to_lower = FALSE,
      drop = FALSE
    ) %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(doc_id) %>%
    dplyr::mutate(sen_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()

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
    dtplyr::lazy_dt() %>%
    dplyr::group_by(doc_id) %>%
    dplyr::filter(!token == "") %>%
    dplyr::mutate(tok_id = dplyr::row_number()) %>%
    dplyr::select(doc_id, pag_id, par_id, sen_id, tok_id, token) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()

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
    dtplyr::lazy_dt() %>%
    dplyr::group_by(doc_id, !!!quos_) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::mutate(sep = paste(!!!quos_, sep = "-")) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble()


  # Calculate Ngram -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  ngrams_ <- sort(unique(.tls$ngram))
  out_ <- purrr::map(ngrams_, ~ get_ngram(doc_, .tls, .x)) %>%
    dplyr::bind_rows() %>%
    dtplyr::lazy_dt() %>%
    dplyr::arrange(doc_id, start, -ngram) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      ids = purrr::map2(start, stop, ~ .x:.y),
      dup = utils::relist(flesh = duplicated(unlist(ids)), skeleton = ids),
      dup = purrr::map_lgl(dup, all)
    ) %>%
    dplyr::select(-ids)

  return(out_)
}


#' Summarize Position Counts
#'
#' @param .tab A Dataframe generated by position_count()
#'
#' @return
#' A Dataframe with the following columns:
#' doc_id: Document ID of the dataframe in argument .document prepared by prep_document()\cr
#' tid: Hash value of the terms in argument .termlist prepared by prep_termlist()\cr
#' n_dup: Count of the term, including N-grams that are part of a higher N-gram\cr()
#' n_uni: Count of the term, excluding N-grams that are part of a higher N-gram\cr()
#' @export
#'
#' @import data.table
summarize_count <- function(.tab) {
  doc_id <- tid <- ngram <- term <- dup <- NULL

  .tab %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(doc_id, tid, ngram, term) %>%
    dplyr::summarise(
      n_dup = dplyr::n(),
      n_uni = sum(!dup),
      .groups = "drop"
    ) %>%
    tibble::as_tibble()
}


#' Get Context of Terms in a Document
#'
#' @param .pos A dataframe with terms and their positions in the input .doc dataframe.
#' @param .doc A tokenized dataframe of an input text.
#' @param .context A character vector specifying the context type, either "word" or "sentence".
#' @param .n A positive integer specifying the number of words or sentences to include in the context.
#' @param .sep A character string used as a separator between sentences when concatenating tokens.
#'
#' @return A dataframe with pre and post context information for each term.
#' @export
get_context <- function(.pos, .doc, .context = c("word", "sentence"), .n, .sep = "---") {

  doc_id <- tid <- ngram <- dup <- start <- pre <- term <- post <-  sen_id <- tmp <-
    token <- tok_id <- pag_id <- par_id <- NULL

  # Check if the input .context is valid and set the context_ variable accordingly
  context_ <- match.arg(.context, c("word", "sentence"))

  # Check if the context_ is not in the allowed values and stop with an error message
  if (!context_ %in% c("word", "sentence")) {
    stop("Invalid .context: Please use 'word' or 'sentence'.")
  }

  # Initialize the "pre" and "post" columns with empty strings
  tab_ <- .pos %>%
    dplyr::mutate(pre = NA_character_, post = NA_character_) %>%
    dplyr::select(doc_id, tid, ngram, dup, start, stop, pre, term, post) %>%
    dplyr::mutate(context = .context, n = .n)

  # Return tab_ if context_ is "word" and .n is 0
  if (context_ == "word" & .n == 0) {
    return(tab_)
  }

  # Modify .doc by adding tmp and token columns
  doc_ <- .doc %>%
    dplyr::mutate(
      tmp = dplyr::if_else(!dplyr::lead(sen_id) == sen_id, paste0(" ", .sep), ""),
      tmp = dplyr::if_else(is.na(tmp), "", tmp),
      token = paste0(token, tmp),
    )


  # If context_ is "word", calculate pre and post ranges for the context
  if (context_ == "word") {
    # Calculate the position of the previous word relative to the current term
    pre1 <- tab_$start - 1L

    # Set the position to 1 if it does not correspond to a token ID in .doc
    pre1[!pre1 %in% .doc$tok_id] <- 1L

    # Calculate the position of the first word in the pre-context
    pre0 <- pre1 - .n + 1

    # Set the position to 1 if it does not correspond to a token ID in .doc
    pre1[!pre0 %in% .doc$tok_id] <- 1L

    # Calculate the position of the first word in the post-context
    post0 <- tab_$stop + 1L

    # Set the position to the maximum token ID if it does not correspond to a token ID in .doc
    post0[!post0 %in% .doc$tok_id] <- max(.doc$tok_id)

    # Calculate the position of the last word in the post-context
    post1 <- post0 + .n - 1

    # Set the position to the maximum token ID if it does not correspond to a token ID in .doc
    post1[!post1 %in% .doc$tok_id] <- max(.doc$tok_id)


    # If context_ is "sentence", calculate pre and post ranges for the context
  } else if (context_ == "sentence") {
    # Calculate minimum and maximum token IDs for each sentence
    sen_min_ <- .doc %>%
      dplyr::group_by(sen_id) %>%
      dplyr::filter(tok_id == min(tok_id)) %>%
      dplyr::ungroup() %>%
      dplyr::select(sen_id, tok_id)

    sen_max_ <- .doc %>%
      dplyr::group_by(sen_id) %>%
      dplyr::filter(tok_id == max(tok_id)) %>%
      dplyr::ungroup() %>%
      dplyr::select(sen_id, tok_id)

    # Calculate the sentence ID for the first word in the pre-context
    pre0 <- tibble::tibble(sen_id = .doc$sen_id[tab_$start] - .n) %>%
      # Join with the sentence IDs of the earliest sentences in .doc
      dplyr::left_join(sen_min_, by = dplyr::join_by(sen_id)) %>%
      # Extract the token ID of the first word in the pre-context
      dplyr::pull(tok_id)

    # Set the token ID to 1 if it is missing
    pre0[is.na(pre0)] <- 1L

    # Calculate the position of the previous word relative to the current term
    pre1 <- tab_$start - 1L

    # Set the position to 1 if it does not correspond to a token ID in .doc
    pre1[!pre1 %in% .doc$tok_id] <- 1L

    # Calculate the token ID of the last word in the post-context
    post1 <- tibble::tibble(sen_id = .doc$sen_id[tab_$stop] + .n) %>%
      # Join with the sentence IDs of the latest sentences in .doc
      dplyr::left_join(sen_max_, by = dplyr::join_by(sen_id)) %>%
      # Extract the token ID of the last word in the post-context
      dplyr::pull(tok_id)

    # Set the token ID to the maximum value in .doc if it is missing
    post1[is.na(post1)] <- max(.doc$tok_id)

    # Calculate the position of the first word in the post-context
    post0 <- tab_$stop + 1L

    # Set the position to the maximum token ID if it does not correspond to a token ID in .doc
    post0[!post0 %in% .doc$tok_id] <- max(.doc$tok_id)
  }

  # Create lists of pre and post context ranges
  lst_pre <- purrr::map2(pre0, pre1, ~ .x:.y)
  lst_pre <- purrr::map(lst_pre, ~ .x[.x > 0 & .x <= max(.doc$tok_id)])
  lst_post <- purrr::map2(post0, post1, ~ .x:.y)
  lst_post <- purrr::map(lst_post, ~ .x[.x > 0 & .x <= max(.doc$tok_id)])

  # Update tab_ with pre and post context information
  out_ <- tab_ %>%
    dplyr::mutate(
      pre = utils::relist(doc_$token[unlist(lst_pre)], lst_pre),
      post = utils::relist(doc_$token[unlist(lst_post)], lst_post),
      pre = purrr::map_chr(pre, ~ paste(.x, collapse = " ")),
      post = purrr::map_chr(post, ~ paste(.x, collapse = " "))
    ) %>%
    dtplyr::lazy_dt() %>%
    dplyr::left_join(
      y = doc_[, c("tok_id", "pag_id", "par_id", "sen_id")],
      by = c("start" = "tok_id")
      ) %>%
    tibble::as_tibble() %>%
    dplyr::relocate(pag_id, par_id, sen_id, .after = doc_id)

  return(out_)
}


#' Get Context of Terms in a Document
#'
#' @param .pos A dataframe with terms and their positions in the input .doc dataframe.
#' @param .doc A tokenized dataframe of an input text.
#' @param .n A positive integer specifying the number of words or sentences to include in the context.
#'
#' @return A dataframe with pre and post context information for each term.
#' @export
get_context_word <- function(.pos, .doc, .n) {
  doc_id <- tid <- ngram <- dup <- start <- pre <- term <- post <-  sen_id <- tmp <-
    token <- tok_id <- pag_id <- par_id <- n <- group_id <- type <- new_sen <-
    new_par <- context <- NULL

  pos_ <- .pos %>%
    dplyr::mutate(n = .n) %>%
    dtplyr::lazy_dt() %>%
    dplyr::group_by(doc_id) %>%
    dplyr::mutate(group_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      pre = purrr::map2(start, n, ~ (.x - 1L):(.x - .y)),
      post = purrr::map2(stop, n, ~ (.x + 1L):(.x + .y))
    ) %>%
    dplyr::select(doc_id, group_id, tid, term, pre, post) %>%
    tibble::as_tibble() %>%
    tidyr::unnest(c(pre, post)) %>%
    tidyr::pivot_longer(c(pre, post), names_to = "type", values_to = "tok_id")  %>%
    dtplyr::lazy_dt() %>%
    dplyr::arrange(doc_id, group_id, type, tok_id) %>%
    tibble::as_tibble()



  out_ <- .doc %>%
    dtplyr::lazy_dt() %>%
    dplyr::inner_join(pos_, by = c("doc_id", "tok_id")) %>%
    dplyr::group_by(doc_id, group_id, term, type) %>%
    dplyr::mutate(
      new_sen = dplyr::lag(sen_id) != sen_id,
      new_par = dplyr::lag(par_id) != par_id,
      dplyr::across(c(new_sen, new_par), ~ dplyr::if_else(is.na(.), FALSE, .))
      ) %>%
    dplyr::mutate(token = dplyr::case_when(
      new_sen & new_par ~ paste0("[ps] ", token),
      new_sen & !new_par ~ paste0("[s] ", token),
      !new_sen & new_par ~ paste0("[p] ", token),
      TRUE ~ token
    )) %>%
    dplyr::arrange(tok_id, .by_group = TRUE) %>%
    dplyr::summarise(
      context = paste(token, collapse = " "),
      .groups = "drop"
    ) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_wider(names_from = type, values_from = context) %>%
    dplyr::select(doc_id, pre, term, post) %>%
    dplyr::mutate(context = "word", n = .n)

  return(out_)
}


