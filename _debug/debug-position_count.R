.tls  <- prep_termlist(table_terms, std_str, FALSE)
.doc  <- dplyr::bind_rows(
  dplyr::mutate(table_doc, doc_id = "BASF-A"),
  dplyr::mutate(table_doc, doc_id = "BASF-B")
)
.doc  <- prep_document(.doc, std_str, "tok")
quos_ <- dplyr::quos(sen_id)
