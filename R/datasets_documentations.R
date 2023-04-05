#' Example Text
#'
#' A dataset containing an example text from wikipedia
#' \href{https://en.wikipedia.org/wiki/Natural_language_processing}{Wikipedia}
#'
#' @format A data frame one row and two columns
#' \describe{
#'   \item{doc_id}{A unique identifier of the document}
#'   \item{text}{Text of the document}
#' }
"table_doc"



#' Example Term List (Short)
#'
#' A dataset containing example terms to be searched within a document
#'
#' @format A data frame 7 rows and 2 columns
#' \describe{
#'   \item{tid}{A term identifier}
#'   \item{term}{A Term}
#'   \item{part_of_ngram }{Indicator Column if a term is part of a higher ngram}
#'   \item{origin}{Term origin}
#' }
"table_terms"
