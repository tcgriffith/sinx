#' Create a pagedown book from a sinx library
#'
#' @param lib library name of the sayings. See `?read.sinxs`.
#' @param title title of the book
#' @param editor editor name
#' @param section name of a sinx libary column, in which the characters are the sections in the book
#' @param subsection name of a sinx libary column, in which the characters are the subsections in the book
#' @param file file name of the book. Without extension.
#' @param if_render if render the book
#'
#' @return a book
#' @importFrom xaringan inf_mr
#' @export
#'
#' @examples
#' \dontrun{
#' secx()
#'
#' secx(lib = 'sinxs', title = 'cosx', subsection = 'date')
#' }
secx <- function(lib = 'tangshi', title = 'A Pagedown Book', editor = 'dapeng', section = 'author', subsection = 'context', file = NULL, if_render = TRUE){
  body_df <- read.sinxs(lib =lib, file = file)
  body_df <- body_df[order(body_df[, section]),]
  body_df[, section][duplicated(body_df[, section])] <- ''
  body_df$section <- ifelse(body_df[, section] == '', '\n', paste('# ', body_df[, section]))
  body_df$subsection <- paste('## ', body_df[, subsection])
  rmd_body <- paste(body_df$section, body_df$subsection, body_df$quote, sep = '\n\n', collapse = '\n\n')
  rmd_body <- strsplit(rmd_body, '\\\\n')[[1]]
  rmd_body2 <- rep('', length(rmd_body) * 2)
  rmd_body2[seq(1, by = 2, length.out = length(rmd_body))] <- rmd_body
  rmd_new <- c("---",
               paste0("title: ", title),
               paste0("author: ", editor),
               "output:",
               "  pagedown::html_paged:",
               "    toc: true",
               "    self_contained: false",
               "paged-footnotes: true",
               "---", rmd_body2)
  filetemp <- paste0(title, '.Rmd')
  writeLines(rmd_new, filetemp, useBytes = TRUE)
  if(if_render) xaringan::inf_mr(filetemp)
}
