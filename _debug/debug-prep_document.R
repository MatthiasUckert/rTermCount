.tab  <- dplyr::bind_rows(
  dplyr::mutate(table_doc, doc_id = "BASF-A"),
  dplyr::mutate(table_doc, doc_id = "BASF-B")
)
.fun_std <- std_str
.until = c("tok", "sen", "par", "pag")
