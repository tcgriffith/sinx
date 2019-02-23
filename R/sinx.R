#' Read R sinx fortunes.
#'
#' @param file a character string giving a sinx fortune database in csv format (in UTF-8 encoding). By default all csv files in the data directory of the sinx package are used.
#'
#' @return a data frame of fortunes, each row contains:
#' - quote:	the quote, main part of the fortune,
#' - author: the author of the quote,
#' - context: the context in which it was quoted (if available, otherwise NA),
#' - source	: where it was quoted (if available, otherwise NA),
#' - date:	when it was quoted (if available, otherwise NA).
#' @importFrom utils read.table
#' @export
#'
#' @examples
#' read.sinxs()
read.sinxs <- function(file = NULL, sep = ','){
  os <- Sys.info()['sysname']
  if(os == 'Windows') {
    old_loc <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE",old_loc))
    Sys.setlocale("LC_CTYPE","English")
  }
  if(!is.null(file)) {
    sinxs <- file[file.exists(file)]
  } else {
    path <- system.file("sinxs", package = "sinx")
    datafiles <- list.files(path)
    if(!is.null(file) && file.exists(file.path(path, file))) {
      sinxs <- file.path(path, file)
    } else {
      if(length(file) > 0L) stop("sorry, ", sQuote(file), " not found")
      file <- datafiles[grep("\\.csv$", datafiles)]
      if(length(file) == 0L) stop("sorry, no sinxs data found")
      sinxs <- file.path(path, file)
    }
  }

  rval <- NULL
  nfile <- length(sinxs)
  sep <- rep_len(sep, nfile)
  for(i in 1:nfile) {
    rval <- rbind(rval, read.table(sinxs[i], header = TRUE, sep = sep[i],
				   quote = "\"", colClasses = "character", encoding = 'UTF-8'))
  }
  rval
}

sinxs.env <- new.env()

#' Find R sinx fortunes.
#'
#' @param which an integer specifying the row number of `sinxs.data`. Alternatively `which`` can be a character and `grep`` is used to try to find a suitable row.
#' @param sinxs.data data frame containing a fortune in each row. By default the data from the 'sinx' package are used.
#' @param fixed logical passed to `grep` if `which`` is a character, indicating if it should work (if `TRUE`, as by default) with a simple character string or (if `FALSE`) with regular expressions.
#' @param showMatches if `which` is character, a logical indicating if `sinx()` should print all the row numbers of `sinxs.data` which match the `grep` search.
#' @param author a character string to match (via `grep`) to the "authors" column of `sinxs.data`.
#' @param ... potential further arguments passed to `grep`.
#'
#' @return an object of class "sinx" which is a row from a data frame of fortunes (like those read in from read.sinxs).
#' @export
#'
#' @examples
#' sinx()
#'
#' for(i in 1:4) print(sinx(i))
#'
#' path_f <- system.file("fortunes/fortunes.csv", package = "fortunes")
#' path_s <- system.file("sinxs/sinxs.csv", package = "sinx")
#' ftns <- sinx::read.sinxs(c(path_f, path_s), sep = c(';', ','))
#' sinx::sinx(sinxs.data = ftns)
sinx <- function(which = NULL, sinxs.data = NULL, fixed = TRUE,
                    showMatches = FALSE, author = character(), ...)
{
  if(is.null(sinxs.data)) {
    if(is.null(sinxs.env$sinxs.data)) sinxs.env$sinxs.data <- read.sinxs()
    sinxs.data <- sinxs.env$sinxs.data
  }

  if(is.null(which) && !length(author)) {
    which <- sample.int(nrow(sinxs.data), 1)
  } else if(is.character(which) || length(author)) {
    if(length(author)) {
      if(is.null(fd.auth <- sinxs.data[, "author"])) {
        warning("'sinxs.data' does not have an \"author\" column")
      } else {
        sinxs.data <- sinxs.data[grep(author, fd.auth, useBytes = TRUE, fixed = fixed), ]
      }
    }
    if(is.character(which)) {
      fort <- apply(sinxs.data, 1, function(x) paste(x, collapse = " "))
      which1 <- grep(which, fort, useBytes = TRUE, fixed = fixed, ...)
      if(length(which1) < 1) which1 <- grep(tolower(which), tolower(fort), useBytes = TRUE, fixed = TRUE, ...)
    } else {
      which1 <- seq_len(nrow(sinxs.data))
    }
    if(showMatches) cat("Matching row numbers:", paste(which1, collapse = ", "), "\n")
    which <- which1
    if(length(which) > 1) which <- sample(which, size = 1)
  }
  if(length(which) > 0 && which %in% seq(along = rownames(sinxs.data))) {
    structure(sinxs.data[which, ], class = "sinx")
  } else {
    character(0)
  }
}

#' Print R sinx fortunes.
#'
#' @param x an object of class "sinx", usually a single row from `sinxs.data`.
#' @param ... potential further arguments passed to `grep`.
#'
#' @return print.
#' @export
print.sinx <- function(x, ...)
{
  # if(is.null(width)) width <- getOption("width")
  # if(width < 10) stop("'width' must be greater than 10")

  x$context <- if(is.na(x$context)) "" else paste(" (", x$context, ")", sep = "")
  if(is.na(x$source)) x$source <- ""
  x$date <- if(is.na(x$date)) "" else x$date <- paste(" (", x$date, ")", sep = "")
  if(anyNA(x)) stop("'quote' and 'author' are required")

  line1 <- x$quote
  line2 <- paste("   -- ", x$author, x$context, sep = "")
  line3 <- paste("      ", x$source, x$date, sep = "")

  # ## Problem: account for chase where line contains "\n"
  # linesplit <- function(line, width, gap = "      ") {
  #   if(nchar(line) < width) return(line)
  #   rval <- NULL
  #   while(nchar(line) > width) {
  #     line <- strsplit(line, " ")[[1]]
  #     if(any((nchar(line) + 1 + nchar(gap)) > width))
  #         stop("'width' is too small for sinx")
  #     breakat <- which.max(cumsum(nchar(line) + 1) > width) - 1L
  #     rval <- paste(rval, paste(line[1:breakat], collapse = " "), "\n", sep = "")
  #     line <- paste(gap, paste(line[-(1:breakat)], collapse = " "), sep = "")
  #   }
  #   paste(rval, line, sep = "")
  # }

  line1 <- strsplit(line1, "\\\\n")[[1]]
  # for(i in 1:length(line1))
  #   line1[i] <- linesplit(line1[i], width, gap = "")
  line1 <- paste(line1, collapse = "\n")
  # line2 <- linesplit(line2, width)
  # line3 <- linesplit(line3, width)

  cat(paste("\n", line1, "\n\n\n",
                  line2, "\n",
                  line3, "\n\n", sep = ""))
}


#' Show a sinx fortune when starting R
#'
#' @return add sinx to ~/.Rprofile
#' @export
#'
#' @examples
#' ctanx()
ctanx <- function(){
  homedir <- Sys.getenv('HOME')
  newcode <- 'sinx::tanx()'
  newfile <- file.path(homedir, '.Rprofile')
  write(newcode, newfile, append = T)
}

merge_text <- function(sinxs.data = NULL, method = c('console', 'vig')){
  method <- match.arg(method)
  if(is.null(sinxs.data)){
    sinxfile <- system.file("sinxs", "sinxs.csv", package = "sinx")
    sinxs.data <- read.sinxs(sinxfile)
  }
  sinxs.data$quote <- gsub('\\\\n', '\n',  sinxs.data$quote)

  os <- Sys.info()['sysname']
  if(os == 'Windows') {
    old_loc <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE",old_loc))
    Sys.setlocale("LC_CTYPE","Chinese")
  }
  n <- nrow(sinxs.data)
  sinxs.data$n <- 1:n
  if(method == 'vig') sinxs.data$vig <- paste(paste0('### ', sinxs.data$n), sinxs.data$quote, paste0('\n\n--- ', sinxs.data$author, ' (', sinxs.data$context, '), ', sinxs.data$source, ', ', sinxs.data$date), sep = '\n\n')
  if(method == 'console') sinxs.data$vig <- paste(sinxs.data$quote, paste0('\n--- ', sinxs.data$author, ' (', sinxs.data$context, '), ', sinxs.data$source, ', ', sinxs.data$date), sep = '\n')
  return(sinxs.data)
}
