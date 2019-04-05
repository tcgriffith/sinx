#' Read sayings from spread sheets.
#'
#' @param file a character string giving a sinx sayings database in csv format (in UTF-8 encoding). By default all csv files in the data directory of the sinx package are used.
#' @param lib library name of the sayings.
#' - 'sinxs': (default) from cosx.org
#' - 'tangshi'
#' - 'songshi'
#' - 'yangsheng'
#' - 'chinese'
#' - 'english'
#' - 'jinyong'
#' @param sep seperator of the columns. See '?read.table()'.
#'
#' @return a data frame of sayings, each row contains:
#' - quote:	the quote, main part of the sayings,
#' - author: the author of the quote,
#' - context: the context in which it was quoted (if available, otherwise NA),
#' - source	: where it was quoted (if available, otherwise NA),
#' - date:	when it was quoted (if available, otherwise NA).
#' @importFrom utils read.table
#' @export
#'
#' @examples
#' libs <- read.sinxs()
#'
#' libs <- read.sinxs(lib = 'jinyong')
#'
#' libs <- read.sinxs(lib = c("tangshi", "songshi", "chinese", "yangsheng", "english","jinyong"))
#'
#' path_f <- system.file("fortunes/fortunes.csv", package = "fortunes")
#' path_s <- system.file("sinxs/sinxs.csv", package = "sinx")
#' libs <- sinx::read.sinxs(c(path_f, path_s), sep = c(";", ","))
read.sinxs <- function(file = NULL,
                       sep = ',',
                       lib = NULL) {
  os <- Sys.info()['sysname']
  if (os == 'Windows') {
    old_loc <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", old_loc))
  }

  allfiles <- dir(system.file("sinxs", package = "sinx"))
  stems = gsub("\\..*$","",allfiles)

  if(is.null(lib)){
    lib = stems
  }

  if (!is.null(file)) {
    sinxs <- file[file.exists(file)]
  }
  else {
    sinxs <- system.file("sinxs", allfiles[stems %in% lib], package = "sinx")
  }

  rval <- NULL
  nfile <- length(sinxs)
  sep <- rep_len(sep, nfile)
  for (i in 1:nfile) {
    afile <- sinxs[i]
    atable <- NULL
    if(grepl('\\.csv$', afile)){
      if (os == 'Windows')
        Sys.setlocale("LC_CTYPE", "English")

      atable <- data.table::fread(sinxs[i])
      print(ncol(atable))
      # atable <- read.table(allowEscapes = TRUE,
      #   sinxs[i],
      #   header = TRUE,
      #   sep = sep[i],
      #   quote = "\"",
      #   colClasses = "character",
      #   encoding = 'UTF-8'
      # )
      if (os == 'Windows')
        Sys.setlocale("LC_CTYPE", old_loc)
    }

    # if(grepl('\\.md$', afile))
    #   atable <- md2df(sinxs[i])

    if(grepl("\\.yaml$", afile)){
      atable <- do.call(rbind, (yaml::read_yaml(afile)))
      atable <- as.data.frame.matrix(atable,stringsAsFactors=FALSE)
      print(ncol(atable))
      # print(atable)
    }

    # rval <- rbind(rval, atable)
  }

  rval
}

sinxs.env <- new.env()

#' Sino Xmen's sayings the R community.
#'
#' @param which an integer specifying the row number of `sinxs.data`. Alternatively `which`` can be a character and `grep`` is used to try to find a suitable row.
#' @param sinxs.data data frame containing a saying in each row. By default the data from the 'sinx' package are used.
#' @param fixed logical passed to `grep` if `which`` is a character, indicating if it should work (if `TRUE`, as by default) with a simple character string or (if `FALSE`) with regular expressions.
#' @param showMatches if `which` is character, a logical indicating if `sinx()` should print all the row numbers of `sinxs.data` which match the `grep` search.
#' @param author a character string to match (via `grep`) to the "authors" column of `sinxs.data`.
#' @param ... potential further arguments passed to `grep`.
#'
#' @return an object of class "sinx" which is a row from a data frame of sayings (like those read in from read.sinxs).
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
#'
#' jinyong <- read.sinxs(lib = 'jinyong')
#' sinx(sinxs.data = jinyong)
#'
#'libs <- read.sinxs(lib = c("tangshi", "songshi", "chinese", "yangsheng", "english","jinyong"))
#'sinx(sinxs.data = libs)

sinx <- function(which = NULL,
                 sinxs.data = NULL,
                 fixed = TRUE,
                 showMatches = FALSE,
                 author = character(),
                 ...)
{
  if (is.null(sinxs.data)) {
    if (is.null(sinxs.env$sinxs.data))
      sinxs.env$sinxs.data <- read.sinxs()
    sinxs.data <- sinxs.env$sinxs.data
  }

  if (is.null(which) && !length(author)) {
    which <- sample.int(nrow(sinxs.data), 1)
  } else if (is.character(which) || length(author)) {
    if (length(author)) {
      if (is.null(fd.auth <- sinxs.data[, "author"])) {
        warning("'sinxs.data' does not have an \"author\" column")
      } else {
        sinxs.data <-
          sinxs.data[grep(author,
                          fd.auth,
                          useBytes = TRUE,
                          fixed = fixed), ]
      }
    }
    if (is.character(which)) {
      fort <- apply(sinxs.data, 1, function(x)
        paste(x, collapse = " "))
      which1 <-
        grep(which, fort, useBytes = TRUE, fixed = fixed, ...)
      if (length(which1) < 1)
        which1 <-
        grep(tolower(which),
             tolower(fort),
             useBytes = TRUE,
             fixed = TRUE,
             ...)
    } else {
      which1 <- seq_len(nrow(sinxs.data))
    }
    if (showMatches)
      cat("Matching row numbers:", paste(which1, collapse = ", "), "\n")
    which <- which1
    if (length(which) > 1)
      which <- sample(which, size = 1)
  }
  if (length(which) > 0 &&
      which %in% seq(along = rownames(sinxs.data))) {
    structure(sinxs.data[which, ], class = "sinx")
  } else {
    character(0)
  }
}

#' Print R sinx sayings.
#'
#' @param x an object of class "sinx", usually a single row from `sinxs.data`.
#' @param ... potential further arguments passed to `grep`.
#'
#' @return print.
#' @export
print.sinx <- function(x)
{
  x$context <-
    if (is.na(x$context) | x$context == '')
      ""
  else
    paste(" (", x$context, ")", sep = "")
  if (is.na(x$source) | x$source == '')
    x$source <- ""
  x$date <-
    if (is.na(x$date) | x$date == '')
      ""
  else
    x$date <- paste(" (", x$date, ")", sep = "")
  if (is.na(x$author) | x$author == '')
    x$author <- "unkown"
  if (anyNA(x))
    stop("'quote' is required")

  lines = character()
  lines[1] <- x$quote
  lines[2] <- paste0("   -- ", x$author, x$context)
  lines[3] <- paste0("      ", x$source, x$date)

  outtxt <- paste0(lines, collapse = "\n")
  cat(outtxt)
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

get_entry <- function(x) {
  x <- strsplit(x, 'ssiinnxx')[[1]]
  entry <- NULL
  for (i in c('author', 'context', 'source', 'date')) {
    loc <- grep(paste0('^', i, ':'), x)
    txt <- gsub(paste0('^', i, ':(.*)'), '\\1', x[loc])
    x <- x[-loc]
    txt <- gsub("^[[:space:]]*|[[:space:]]*$", "", txt)
    entry <- c(entry, txt)
  }
  # merge the rest
  quoteloc <- which(x != '')
  quotetxt <- x[min(quoteloc):max(quoteloc)]
  quotetxt <- paste0(quotetxt, collapse = '\n')
  c(quotetxt, entry)
}

