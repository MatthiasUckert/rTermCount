
# tmp_ <- tibble::as_tibble(cnt2_)[["tok_id"]]
cnt2_[["dup"]] <- duplicated(cnt2_[["tok_id"]])
cnt1_[["dup"]] <- cnt1_[["start"]] %in% cnt2_[["tok_id"]]
# Prepare Output -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
tab_ <- term_list_ %>%
  dplyr::select(hash, ngram, term, oid, token) %>%
  tidyr::unnest(c(oid, token)) %>%
  dtplyr::lazy_dt() %>%
  dplyr::inner_join(tab_pos_, by = "token") %>%
  dplyr::arrange(hash, pos, oid) %>%
  dplyr::group_by(hash, tmp = cumsum(oid == 1)) %>%
  dplyr::filter(dplyr::row_number() <= ngram) %>%
  dplyr::filter(dplyr::n() == ngram) %>%
  dplyr::filter(all(c(1, diff(oid)) == 1) , all(c(1, diff(pos)) == 1)) %>%
  dplyr::arrange(dplyr::desc(ngram), hash)
tab_[["dup"]] <- duplicated(tab_$pos)
tab_ <- tab_ %>%
  dplyr::summarise(
    start = dplyr::first(pos),
    stop = dplyr::last(pos),
    dup  = any(dup),
    doc_id = doc_[["doc_id"]][1],
    .groups = "drop"
  ) %>%
  dplyr::select(-tmp)




cnt2_ <- tls2_ %>%
  dplyr::select(hash, ngram, term, oid, tok = token) %>%
  tidyr::unnest(c(oid, tok)) %>%
  dtplyr::lazy_dt() %>%
  dplyr::inner_join(doc_, by = "token") %>%
  # dplyr::arrange(hash, pos, oid) %>%
  tibble::as_tibble() %>%
  dplyr::slice(1:10000)
dplyr::group_by(hash, tmp = cumsum(oid == 1)) %>%
  dplyr::filter(dplyr::row_number() <= ngram) %>%
  dplyr::filter(dplyr::n() == ngram) %>%
  dplyr::filter(all(c(1, diff(oid)) == 1) , all(c(1, diff(pos)) == 1)) %>%
  dplyr::arrange(dplyr::desc(ngram), hash)
tab_[["dup"]] <- duplicated(tab_$pos)
tab_ <- tab_ %>%
  dplyr::summarise(
    start = dplyr::first(pos),
    stop = dplyr::last(pos),
    dup  = any(dup),
    doc_id = doc_[["doc_id"]][1],
    .groups = "drop"
  ) %>%
  dplyr::filter(sep1 == sep2, term == check) %>%
  dplyr::select(-sep1, -sep2, -gtok, -goid, -check) %>%
  tibble::as_tibble()


tls2_ <- dplyr::filter(.tls, ngram == 2)
tls2_ <- dplyr::select(tls2_, hash, term, ngram, oid, token)

cnt2_ <- .doc %>%
  dplyr::filter(token %in% unlist(tls2_$token))


# Get >1-Grams -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
tls2_ <- dplyr::filter(.tls, ngram > 1)
tls2_ <- tls2_ %>%
  dplyr::select(hash, ngram, token, oid, term) %>%
  tidyr::unnest(c(token, oid))


# Filter Dataframe for only tokens in vec AND for only consecutive tok_ids
cnt2_ <- .doc %>%
  dtplyr::lazy_dt() %>%
  dplyr::filter(token %in% tls2_$token) %>%
  dtplyr::lazy_dt() %>%
  dplyr::left_join(tls2_, by = "token") %>%
  dplyr::arrange(hash, tok_id, oid) %>%
  tibble::as_tibble() %>%
  # dplyr::group_by(hash) %>%
  keep_groups(.group = "tok")  %>%
  dplyr::mutate(keep = c(1L, diff(oid))) %>%
  dplyr::mutate(keep = dplyr::if_else(dplyr::lead(keep == 1L), 1L, as.integer(keep))) %>%
  dplyr::mutate(keep = dplyr::if_else(keep == 1L, 1L, NA_integer_)) %>%
  dplyr::mutate(goid = cumsum(is.na(keep)))
#
#   ,
#       ,
#       keep = ,
#       goid = cumsum(is.na(keep))
#     ) %>%
#     dplyr::filter(!is.na(keep)) %>%
#     dplyr::select(-keep)
#
#
#     keep_groups(.group = "oid")  %>%
#     dtplyr::lazy_dt() %>%
#     dplyr::group_by(gtok, goid) %>%
#     dplyr::filter(dplyr::n() == ngram) %>%
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
  )  %>%
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



f1 <- function() {
  pos_ <- purrr::map(vec_, ~ which(.x == doc_$token))
  pos_ <- tibble::enframe(pos_, name = "tok", value = "pos")
  pos_ <- tidyr::unnest(pos_, pos)
  pos_ <- dplyr::mutate(pos_, pos = doc_[["tok_id"]][pos])
  pos_ <- dplyr::arrange(pos_, tok, pos)
  return(pos_)
}

f2 <- function() {
  pos_ <- purrr::map(vec_, ~ which(.x == doc_$token))
  pos_ <- dplyr::select(tibble::as_tibble(stack(pos_, )), tok = ind, pos = values)
  pos_[["pos"]] <- doc_[["tok_id"]][pos_[["pos"]]]
  pos_[["tok"]] <- as.character(pos_[["tok"]])
  pos_ <- dplyr::arrange(pos_, tok, pos)
}

f3 <- function() {
  pos_ <- tibble::tibble(tok = vec_)
  pos_ <- dtplyr::lazy_dt(pos_)
  pos_ <- dplyr::inner_join(pos_, dplyr::select(doc_, tok = token, pos = tok_id), by = "tok")
  pos_ <- dplyr::arrange(pos_, pos)
  pos_ <- tibble::as_tibble(pos_)
}



t1 <- f1()
t2 <- f2()
t3 <- f3()
bench_ <- bench::mark(f1(), f2(), f3(), iterations = 2)[, 1:9]






tictoc::tic()
tab1_ <- purrr::map_dfr(
  .x = sort(unique(.tls$ngram))[1:10],
  .f = ~ get_ngram(doc_, .tls, .x)
)
tictoc::toc()

tictoc::tic()
tls_ <- dtplyr::lazy_dt(.tls)
doc_ <- dtplyr::lazy_dt(.doc)
tab_ <- purrr::map_dfr(
  .x = sort(unique(tls_$ngram)),
  .f = ~ get_ngram(doc_, .tls, .x)
)
tictoc::toc()

# Get 1-Grams -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
tls1_ <- dplyr::filter(.tls, ngram == 1)
tls1_ <- dplyr::select(tls1_, hash, term, ngram)
cnt1_ <- .doc %>%
  dplyr::rename(term = token) %>%
  dtplyr::lazy_dt() %>%
  dplyr::inner_join(tls1_, by = "term") %>%
  dplyr::select(doc_id, hash, ngram, term, start = tok_id, stop = tok_id) %>%
  tibble::as_tibble()

# Get >1-Grams -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
tls2_ <- dplyr::filter(.tls, ngram > 1) %>%
  dplyr::select(hash, ngram, term, oid, token) %>%
  tidyr::unnest(c(oid, token))

# get Positions -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
doc_ <- dplyr::filter(.doc, token %in% tls2_$token)

# Prepare Output -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
tictoc::tic()
cnt2_ <- doc_ %>%
  dtplyr::lazy_dt() %>%
  dplyr::inner_join(tls2_, by = "token") %>%
  dplyr::arrange(hash, tok_id, oid) %>%
  tibble::as_tibble()
tictoc::toc()

tictoc::tic()
cnt2_ <- cnt2_ %>%
  dtplyr::lazy_dt() %>%
  dplyr::group_by(hash, tmp = cumsum(oid == 1)) %>%
  dplyr::filter(dplyr::row_number() <= ngram) %>%
  dplyr::filter(dplyr::last(tok_id) - dplyr::first(tok_id) == ngram - 1 & dplyr::n() == ngram) %>%
  dplyr::ungroup() %>%
  tibble::as_tibble()
tictoc::toc()

tictoc::tic()
cnt2_ <- cnt2_ %>%
  dplyr::mutate(
    dup = duplicated(tok_id),
    sep = paste(!!!quos_, sep = "-")
  )
tictoc::toc()

cnt1_[["dup"]] <- cnt1_[["start"]] %in% cnt2_[["tok_id"]]
tictoc::tic()
cnt2_ <- cnt2_ %>%
  dtplyr::lazy_dt() %>%
  dplyr::group_by(hash, tmp) %>%
  dplyr::summarise(
    doc_id = doc_id[1], hash = hash[1], ngram = ngram[1], term = term[1],
    check = paste(token, collapse = " "),
    start = min(tok_id), stop = max(tok_id),
    sep1 = dplyr::first(sep), sep2 = dplyr::last(sep),
    dup = any(dup),
    ids = list(tok_id),
    .groups = "drop"
  ) %>%
  dplyr::filter(sep1 == sep2, term == check) %>%
  dplyr::select(doc_id, hash, ngram, term, start, stop, dup) %>%
  tibble::as_tibble() %>%
  dplyr::bind_rows(cnt1_) %>%
  dtplyr::lazy_dt() %>%
  dplyr::arrange(start) %>%
  tibble::as_tibble()
tictoc::toc()
