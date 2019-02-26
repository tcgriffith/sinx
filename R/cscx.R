#' Create a Skeleton in a Clipboard for a new sinX
#'
#' @return a skeleton text in the clipboard
#' @export
#'
#' @examples
#' cscx()
cscx <- function() {
  skeleton <- c("",
                "",
                "author: ",
                "",
                "context: ",
                "",
                "source: ",
                "",
                "date:",
                "",
                "---")
  writeLines(skeleton, 'clipboard')
}

get_entry <- function(x) {
  x <- strsplit(x, 'ssiinnxx')[[1]]
  entry <- NULL
  for (i in c('author', 'context', 'source', 'date')) {
    loc <- grep(paste0('^', i, ':'), x)
    txt <- gsub(paste0('^', i, ':(.*)'), '\\1', x[loc])
    x <- x[-loc]
    txt <- rosr:::rm_space(txt)
    entry <- c(entry, txt)
  }
  # merge the rest
  quoteloc <- which(x != '')
  quotetxt <- x[min(quoteloc):max(quoteloc)]
  quotetxt <- paste0(quotetxt, collapse = '\n')
  c(quotetxt, entry)
}

#' create sinx data spread sheet
#'
#' @param mdfile filename of the original .md file
md2df <- function(mdfile) {
  oldtxt <- readLines(mdfile, encoding = 'UTF-8')
  # oldtxt <- oldtxt[oldtxt!='']
  oldtxt <- paste(oldtxt, collapse = 'ssiinnxx')
  singles <- strsplit(oldtxt, 'ssiinnxx---ssiinnxx')[[1]]
  newtxt <- lapply(singles, get_entry)
  newtxt <-
    as.data.frame(matrix(unlist(newtxt), ncol = 5, byrow = T))
  names(newtxt) <- c('quote', 'author', 'context', 'source', 'date')
  newtxt
}